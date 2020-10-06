{-# LANGUAGE TupleSections #-}

module Rejig.IESorter where

import Rejig.Ast
import Rejig.Pretty (showPretty, Pretty)
import Rejig.Settings
import Text.PrettyPrint (render)
import Rejig.Lang (runReader')
import qualified Data.Text as T

import qualified Data.List as L


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

sortImports :: ImportDecls -> Reader Settings ImportDeclGroups
sortImports (ImportDecls decls) = do
  settings <- ask
  pure $ ImportDeclGroups . map (subsortGroup settings) $ topLevelGroups settings decls

topLevelGroups :: Settings -> [ImportDecl] -> [[ImportDecl]]
topLevelGroups _ =
  L.groupBy (eq `on` weight) . (L.sortBy (by weight) =<<)
  . subGroupByPkgQual
  . partitionByPkgQual
 where
  -- partition into 2 groups (normal imports, package imports)
  partitionByPkgQual :: [ImportDecl] -> [[ImportDecl]]
  partitionByPkgQual =
    L.groupBy (eq `on` pkgQualWeight) . L.sortBy (by pkgQualWeight)

  -- within the package imports partition, imports with the same qualifier get grouped together
  subGroupByPkgQual :: [[ImportDecl]] -> [[ImportDecl]]
  subGroupByPkgQual =
    L.groupBy (eq `on` getPkgQual) . (L.sortBy (by getPkgQual) =<<)
    where
      getPkgQual =
        maybe "" id . ideclPkgQual

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




-- ieWeight :: IE -> Int
-- ieWeight = \case
  -- IEVar v = weight

-- data ImportDecl = ImportDecl
 -- { ideclName :: Qual
 -- -- ^ A ModuleName is essentially a string e.g. Data.List
 -- , ideclPkgQual :: Maybe Text
 -- -- ^ Package qualifier
 -- , ideclIsQual :: Bool
 -- -- ^ Does the qualified keyword appear
 -- , ideclAs :: Maybe Qual
 -- -- ^ as Module
 -- , ideclHiding :: Maybe (Bool, [IE])
 -- -- ^ (True => hiding, names)
 -- } deriving (Show, Eq)

-- data CName
  -- = CVarId VarId
  -- | CVarSym VarSym
  -- | CConId ConId
  -- | CConSym ConSym
  -- deriving (Show, Eq)

-- data Var
  -- = VId VarId
  -- | VSym VarSym
  -- deriving (Show, Eq)

-- data IE
  -- = IEVar Var
  -- -- ^ Imported or Exported Variable
  -- | IEThingAbs ConId
  -- -- ^ Imported or exported Thing with Absent list, eg: Month ()
  -- | IEThingAll ConId
  -- -- ^ ClassType plus all methods/constructors, eg: Month(..)
  -- | IEThingWith ConId [CName]
  -- -- ^ ClassType plus some methods/constructors eg: Month(Jan, Feb)
    -- deriving (Show, Eq)

-- instance Ord Var where
  -- compare (VId (VarId a)) (VId (VarId b)) = a == b
  -- compare (VSym (VarId a)) (VId (VarId b)) = a == b



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
