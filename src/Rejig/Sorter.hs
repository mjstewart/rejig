{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Rejig.Sorter (sortParsedSource) where

import Data.Bifunctor (first)
import qualified Data.List as L
import qualified Data.Map as Map
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
sortExports ies = do
  let
    res = (id =<<) . sortGroups $ sortTopLevel ies
  pure res
  where
    sortTopLevel = L.groupBy (eq `on` topLevelWeight) . L.sortBy (by topLevelWeight)
    sortGroups = map sortIEs

sortImports :: ImportDecls -> Reader Settings PartitionedImports
sortImports (ImportDecls decls) = do
  settings <- ask
  (_piPrefixTargets, rest) <- first (map $ fmap (sortGroups settings)) <$> (byPrefixPartition decls)

  let (_piPkgQuals, rest') = first (CG (Just "package qualified") . sortGroups settings) $ pkgQualGroups rest
      _piRest = CG (Just "standard imports") $ sortGroups settings [rest']

  pure $
    PartitionedImports
      { _piPrefixTargets,
        _piPkgQuals,
        _piRest
      }
  where
    sortGroups :: Settings -> [[ImportDecl]] -> ImportDeclGroups
    sortGroups settings =
      ImportDeclGroups . map (sortSubgroup settings) . sortTopLevel


-- | (subgrouped pgk quals, rest)
pkgQualGroups :: [ImportDecl] -> ([[ImportDecl]], [ImportDecl])
pkgQualGroups =
  first subgroup . L.partition (isJust . ideclPkgQual)
  where
    subgroup :: [ImportDecl] -> [[ImportDecl]]
    subgroup =
      L.groupBy (eq `on` getPkgQual) . L.sortBy (by getPkgQual)

    getPkgQual =
      maybe "" id . ideclPkgQual

-- | Groups together any user defined prefixes
byPrefixPartition :: [ImportDecl] -> Reader Settings ([CG [[ImportDecl]]], [ImportDecl])
byPrefixPartition decls = do
  settings <- ask
  pure $
    foldr
      ( \prefix (acc, rest) ->
          first (mergeTargets prefix acc) $ L.partition (T.isPrefixOf prefix . declName settings) rest
      )
      ([], decls)
      $ _sGroupByPrefix settings
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
    . L.sortBy
      (comparingBy [by (renderPretty settings . ideclName)])
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

sortIEThingWith :: IE -> IE
sortIEThingWith = \case
  IEThingWith conid names -> IEThingWith conid (sort names)
  x -> x

class TopLevelWeight x where
  topLevelWeight :: x -> Int

instance TopLevelWeight ImportDecl where
  topLevelWeight decl =
    sum
      [ qualWeight,
        asWeight,
        ieListWeight
      ]
    where
      -- Highest weights get placed last
      qualWeight = bool 0 10000 $ ideclIsQual decl
      asWeight = maybe 0 (const 1000) $ ideclAs decl

      ieListWeight =
        maybe
          0
          ( \(isHiding, names) ->
              -- import Apple () == empty names, slightly increase weight to push it down
              if null names
                then 500
                else -- hiding ordered first
                  bool 4000 2000 isHiding
          )
          $ ideclHiding decl

qWeight :: Int -> Qual -> Int
qWeight base (Qual quals x) =
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
