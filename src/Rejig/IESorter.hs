{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Rejig.IESorter (sortImports) where

import Data.Bifunctor (first)
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import Rejig.Ast
import Rejig.Lang (runReader')
import Rejig.Pretty (Pretty, showPretty)
import Rejig.Settings
import Text.PrettyPrint (render)

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
    >>= L.groupBy (eq `on` weight) . L.sortBy (by weight)
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
      pure . (fmap . fmap) ieSort

    ieSort :: [IE] -> [IE]
    ieSort =
      (>>= sort . map sortIEThingWith)
        . L.groupBy (eq `on` weight)
        . L.sortBy (by weight)

    sortIEThingWith :: IE -> IE
    sortIEThingWith = \case
      IEThingWith conid names -> IEThingWith conid (sort names)
      x -> x

class Weight x where
  weight :: x -> Int

instance Weight ImportDecl where
  weight decl =
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

-- really should be TopLevelGroupWeight?
instance Weight Var where
  weight = \case
    VId (VarId _) -> 100
    VSym (VarSym x) -> 200

instance Weight CName where
  weight = \case
    CVarId _ -> 100
    CVarSym (VarSym x) -> 200
    CConId _ -> 300
    CConSym (ConSym x) -> 400

instance Weight IE where
  weight = \case
    IEVar x -> weight x
    IEThingAbs _ -> 300
    IEThingAll _ -> 400
    IEThingWith _ _ -> 500
