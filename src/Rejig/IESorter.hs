{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Rejig.IESorter where

import Rejig.Ast
import Rejig.Pretty (showPretty, Pretty)
import Rejig.Settings
import Text.PrettyPrint (render)
import Rejig.Lang (runReader')
import qualified Data.Text as T

import qualified Data.List as L
import qualified Data.Map as Map
import Data.Bifunctor (first)


-- flatten :: TopLevelPartition -> ImportDeclGroups
-- flatten p =
  -- _partitionPrefixTargetGroups p -- ++ _partitionPrefixTargetGroups p ++ _partitionPkgGroups p

-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]

-- comparing :: Ord a => (b -> a) -> b -> b -> Ordering
-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]

comparingBy:: [a -> a -> Ordering] -> a -> a -> Ordering
comparingBy fs a b =
  mconcat $ [(\x -> x a b)] <*> fs

on' :: (a -> a -> c) -> (b -> a) -> b -> b -> c
on' f g a b =
  f (g a) (g b)

by :: Ord a => (b -> a) -> b -> b -> Ordering
by f = comparing f

eq :: Eq a => a -> a -> Bool
eq = (==)

renderPretty :: Pretty a => Settings -> a -> String
renderPretty settings =
  render . runReader' settings . showPretty

-- topLevelGroupBy :: Ord b => (b -> a) -> [b] -> [[a]]
-- topLevelGroupBy f =
  -- L.groupBy (eq `on'` f) . L.sortBy (by f)

topLevelPartition :: [ImportDecl] -> Reader Settings PartitionedImports
topLevelPartition decls = do
  settings <- ask
 -- ([CG [[ImportDecl]]], [ImportDecl])
  -- (prefixTargetGroups, rest) <- partitionByPrefixes decls
  (_piPrefixTargetGroups, rest) <- first (map $ fmap (sortPartitionGroup settings)) <$> (partitionByPrefixes decls)

  let
    _piPkgGroups = []
    _piRest = []
    -- m = map (fmap (process settings)) prefixTargetGroups
    (pkgQuals, rest') = pkgQualGroups rest

  pure $ PartitionedImports {..}
  -- pure $ PartitionedImports
   -- { _partitionPrefixTargetGroups =
   -- , _partitionPkgGroups = pkgQuals
   -- , _partitionRest = [rest']
   -- }
   where
    sortPartitionGroup :: Settings -> [[ImportDecl]] -> ImportDeclGroups
    sortPartitionGroup settings =
      ImportDeclGroups . map (subsortGroup settings) . topLevelGroups2

consTuple :: ([[a]], [a]) -> [[a]]
consTuple (a, b) = b : a

-- | (package import groups sorted by import qualifier, rest)
pkgQualGroups :: [ImportDecl] -> ([[ImportDecl]], [ImportDecl])
pkgQualGroups =
  first subgroup . L.partition (isJust . ideclPkgQual)
  where
  -- sort and group by the package import qualifier
  subgroup :: [ImportDecl] -> [[ImportDecl]]
  subgroup =
    L.groupBy (eq `on` getPkgQual) . L.sortBy (by getPkgQual)

  getPkgQual =
    maybe "" id . ideclPkgQual

{-
  ["celery.b", "apple.e", "celery.a", "apple.a"]

  celery

-}
partitionByPrefixes :: [ImportDecl] -> Reader Settings ([CG [[ImportDecl]]], [ImportDecl])
partitionByPrefixes decls = do
  settings <- ask
  pure $ foldr
    (\prefix (acc, rest) ->
      first (mergeTargets prefix acc) $ L.partition (T.isPrefixOf prefix . declName settings) rest
    ) ([], decls) (groupByPrefix settings)
  where
    declName :: Settings -> ImportDecl -> Text
    declName settings = T.pack . renderPretty settings . ideclName

    mergeTargets :: Text -> [CG [[ImportDecl]]] -> [ImportDecl] -> [CG [[ImportDecl]]]
    mergeTargets prefix acc targets =
      if null targets then acc
      else
        CG
          { _cgComment = Just $ "imports by " <> prefix <> "*"
          , _cgGroup = [targets]
          }
        : acc


sortImports :: ImportDecls -> Reader Settings PartitionedImports
sortImports (ImportDecls decls) = do
  settings <- ask
  topLevelPartitions <- topLevelPartition decls
  pure topLevelPartitions
  -- pure $ flatten topLevelPartitions
  -- pure $ ImportDeclGroups . map (subsortGroup settings) $ topLevelGroups settings decls

-- topLevelGroup2 :: [ImportDecl] -> [[ImportDecl]]
-- topLevelGroups =
  -- L.groupBy (eq `on` weight) . (L.sortBy (by weight) =<<)
  -- . subGroupByPkgQual
  -- . partitionByPkgQual


  -- pure $ ImportDeclGroups $ map (subsortGroup settings) $ topLevelGroups2 decls

-- process :: [[ImportDecl]] -> Reader Settings ImportDeclGroups
-- process decls = do
  -- settings <- ask
  -- pure $ ImportDeclGroups $ map (subsortGroup settings) $ topLevelGroups2 decls


  -- pure $ ImportDeclGroups . map (subsortGroup settings) $ topLevelGroups settings decls



topLevelGroups2 :: [[ImportDecl]] -> [[ImportDecl]]
topLevelGroups2 decls = decls
  >>= consTuple . pkgQualGroups
  >>= L.groupBy (eq `on` weight) . L.sortBy (by weight)

ieListLength :: ImportDecl -> Int
ieListLength =
  maybe 0 (length . snd) . ideclHiding

subsortGroup :: Settings -> [ImportDecl] -> ImportDecls
subsortGroup settings =
  ImportDecls . L.sortBy
    (comparingBy [ by (renderPretty settings . ideclName)])
    . map sortSpecificImports
  where
    sortSpecificImports :: ImportDecl -> ImportDecl
    sortSpecificImports decl =
      decl
       { ideclHiding = runReader' settings $ ideclHidingSort $ ideclHiding decl
       }

    ideclHidingSort :: Maybe (Bool, [IE]) -> Reader Settings (Maybe (Bool, [IE]))
    ideclHidingSort =
      pure . (fmap.fmap) ieSort

    ieSort :: [IE] -> [IE]
    ieSort =
      (>>= sort . map sortIEThingWith)
      . L.groupBy (eq `on` weight) . L.sortBy (by weight)

    sortIEThingWith :: IE -> IE
    sortIEThingWith = \case
      IEThingWith conid names -> IEThingWith conid (sort names)
      x -> x






pkgQualWeight :: ImportDecl -> Int
pkgQualWeight =
  maybe 0 (const 1) . ideclPkgQual

class Weight x where
  weight :: x -> Int

instance Weight ImportDecl where
  weight decl =
    sum
    [ qualWeight
    , asWeight
    , ieListWeight
    ]
    where
      -- Highest weights get placed last
      qualWeight = bool 0 10000 $ ideclIsQual decl
      asWeight = maybe 0 (const 1000) $ ideclAs decl

      ieListWeight = maybe 0 (\(isHiding, names) ->
        -- import Apple () == empty names, slightly increase weight to push it down
        if null names then 500
        -- hiding ordered first
        else bool 4000 2000 isHiding
        ) $ ideclHiding decl

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