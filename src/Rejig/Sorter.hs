{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Rejig.Sorter (sortParsedSource) where

import qualified Data.List as L
import qualified Data.Text as T
import Rejig.Ast
import Rejig.Lang (runReader')
import Rejig.Pretty (Pretty, showPretty)
import Rejig.Settings
import Text.PrettyPrint (render)

comparingBy :: [a -> a -> Ordering] -> a -> a -> Ordering
comparingBy fs a b =
  mconcat $ [(\x -> x a b)] <*> fs

by :: Ord a => (b -> a) -> b -> b -> Ordering
by f = comparing f

eq :: Eq a => a -> a -> Bool
eq = (==)

renderPretty :: Pretty a => Settings -> a -> String
renderPretty settings =
  render . runReader' settings . showPretty

-- | Main entry point to sorting the module header of the parsed source file
sortParsedSource :: ParsedSource -> Reader Settings SortedParsedSource
sortParsedSource ps = do
  _ssrcModHeader <- sortModHeader $ _leadingThing $ _srcModHeader ps

  pure $ SortedParsedSource
   { _ssrcModHeader = const _ssrcModHeader <$> (_srcModHeader ps)
   , _ssrcRest = _srcRest ps
   }

sortModHeader :: ModuleHeader -> Reader Settings SortedModuleHeader
sortModHeader mh = do
  sortedImports <- sortImports $ _modImports mh
  sortedExports <- sortExports $ _modExports mh

  pure $ SortedModuleHeader
    { _smodLangExts = sort $ _modLangExts mh
    , _smodGhcOpts = sort $ _modGhcOpts mh
    , _smodName = _modName mh
    , _smodExports = sortedExports
    , _smodImports = sortedImports
    }

sortExports :: [IE] -> Reader Settings [IE]
sortExports =
  pure . (id =<<) . sortGroups . sortTopLevel
  where
    sortTopLevel = L.groupBy (eq `on` topLevelWeight) . L.sortBy (by topLevelWeight)
    sortGroups = map sortIEs

sortImports :: ImportDecls -> Reader Settings PartitionedImports
sortImports (ImportDecls decls) = do
  settings <- ask

  -- sorts each of the partitioned prefix groups
  (_piPrefixTargets, rest) <- first (map $ fmap (sortGroups settings)) <$> (byPrefixPartition decls)

  let
      -- sorts each of the partitioned pkg qualfified groups
      (_piPkgQuals, rest') = first (CG (Just "package qualified") . sortGroups settings) $ pkgQualGroups rest

      _piRest = CG (Just "standard imports") $ sortGroups settings [rest']

  pure $
    PartitionedImports
      { _piPrefixTargets
      , _piPkgQuals
      , _piRest
      }
  where
    sortGroups :: Settings -> [[ImportDecl]] -> ImportDeclGroups
    sortGroups settings =
      ImportDeclGroups . map (sortSubgroup settings) . sortTopLevel

-- | partition pkg qualified decls
pkgQualGroups :: [ImportDecl] -> ([[ImportDecl]], [ImportDecl])
pkgQualGroups =
  first subgroup . L.partition (isJust . ideclPkgQual)
  where
    subgroup :: [ImportDecl] -> [[ImportDecl]]
    subgroup =
      L.groupBy (eq `on` getPkgQual) . L.sortBy (by getPkgQual)

    getPkgQual =
      maybe "" id . ideclPkgQual

-- | group together any user defined prefixes
byPrefixPartition :: [ImportDecl] -> Reader Settings ([CG [[ImportDecl]]], [ImportDecl])
byPrefixPartition decls = do
  settings <- ask
  pure $
    sortByTitle $
      foldr
        (\prefix (acc, rest) ->
            first (mergeTargets prefix acc) $ L.partition (T.isPrefixOf prefix . declName settings) rest
        )
        ([], decls)
        $ _sPrefixGroups settings
  where
    declName :: Settings -> ImportDecl -> Text
    declName settings = T.pack . renderPretty settings . ideclName

    mergeTargets :: Text -> [CG [[ImportDecl]]] -> [ImportDecl] -> [CG [[ImportDecl]]]
    mergeTargets prefix acc targets =
      if null targets
        then acc
        else
          CG
            { _cgComment = Just $ "imports by " <> prefix <> "*",
              _cgGroup = [targets]
            } :
          acc

    sortByTitle :: ([CG [[ImportDecl]]], [ImportDecl]) -> ([CG [[ImportDecl]]], [ImportDecl])
    sortByTitle =
      first $ L.sortBy (by (maybe "" id . _cgComment))

sortTopLevel :: [[ImportDecl]] -> [[ImportDecl]]
sortTopLevel decls =
  decls
    >>= withPkgQualsLast . pkgQualGroups
    >>= L.groupBy (eq `on` topLevelWeight) . L.sortBy (by topLevelWeight)
  where
    withPkgQualsLast (pkgs, rest) = rest : pkgs

sortSubgroup :: Settings -> [ImportDecl] -> ImportDecls
sortSubgroup settings =
  ImportDecls
    . L.sortBy (comparingBy [by (renderPretty settings . ideclName)])
    . map sortSpecificImports
  where
    sortSpecificImports :: ImportDecl -> ImportDecl
    sortSpecificImports decl =
      decl
        { ideclHiding = runReader' settings $ ideclHidingSort $ ideclHiding decl
        }

    ideclHidingSort :: Maybe (Bool, [IE]) -> Reader Settings (Maybe (Bool, [IE]))
    ideclHidingSort =
      pure . (fmap . fmap) sortIEs


sortIEs :: [IE] -> [IE]
sortIEs =
  (>>= sort . map sortIEThingWith)
    . L.groupBy (eq `on` topLevelWeight)
    . L.sortBy (by topLevelWeight)
  where
    sortIEThingWith :: IE -> IE
    sortIEThingWith = \case
      IEThingWith conid names -> IEThingWith conid (sort names)
      x -> x

{-| Sorting is a little bit complicated so its worth explaining how it works.
  Theres lots of components to an import or export declaration so we give each decl a 'weighting score'
  which determines its top level group positioning in the final output.

  eg think 'qualified' keyword is something we might want to make a group of imports out of
  but then there could also be 'as' and 'hiding' keywords that we also want to group by.

  The end result is that the weightings form the top level groups

  eg: [imports] -> [[imports]]

  other functions then map over each top level group and peform a detailed subsort.
-}

class TopLevelWeight x where
  topLevelWeight :: x -> Int

instance TopLevelWeight ImportDecl where
  -- higher values appear last
  topLevelWeight decl =
    sum
      [ qualWeight,
        asWeight,
        ieNamesWeight
      ]
    where
      -- qualified is a bigger compound keyword group so its given a higher weight.
      qualWeight = bool 0 10000 $ ideclIsQual decl
      -- but then we have smaller keywords that can be subgrouped through a smaller weight
      asWeight = maybe 0 (const 1000) $ ideclAs decl

      -- names is something in parens section (a, b, c) - "import Apple (a, b, c)"
      ieNamesWeight =
        maybe
          0
          ( \(isHiding, names) ->
            {-
               Giving Bob the +500 pushes an import with no names down into its own group

               import Alice
               import Bob ()
            -}
              if null names then 500
              else
                -- hiding ordered first
                bool 4000 2000 isHiding
          )
          $ ideclHiding decl

qWeight :: Int -> Qual -> Int
qWeight base (Qual quals _) =
  if null quals then base else base + 10

instance TopLevelWeight QVar where
  topLevelWeight = \case
    VId (QVarId q) -> qWeight 100 q
    VSym (QVarSym q) -> qWeight 200 q

instance TopLevelWeight CName where
  topLevelWeight = \case
    CVarId (QVarId q) -> qWeight 100 q
    CVarSym (QVarSym q) -> qWeight 200 q
    CConId (QConId q) -> qWeight 300 q
    CConSym (QConSym q) -> qWeight 400 q

instance TopLevelWeight IE where
  topLevelWeight = \case
    IEVar x -> topLevelWeight x
    IEThingAbs _ -> 300
    IEThingAll _ -> 400
    IEThingWith _ _ -> 500
    IEModuleContents _ -> 600
