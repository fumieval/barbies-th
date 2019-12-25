{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Barbie.TH (FieldNamesB(..)
  , declareBareB
  ) where

import Language.Haskell.TH hiding (cxt)
import Language.Haskell.TH.Syntax (VarBangType)
import Data.String
import Data.Foldable (foldl')
import Data.Barbie
import Data.Barbie.Bare
import Data.Functor.Product
import GHC.Generics (Generic)
import Control.Applicative
import Data.Functor.Identity (Identity(..))

-- | barbies doesn't care about field names, but they are useful in many use cases
class FieldNamesB b where
  -- | A collection of field names.
  bfieldNames :: IsString a => b (Const a)

-- | Transform a regular Haskell record declaration into HKD form.
-- 'BareB', 'FieldNamesB', 'FunctorB', 'TraversableB', 'ProductB',
-- 'ConstraintsB' and 'ProductBC' instances are derived.
--
-- For example,
--
-- @declareBareB [d|data User = User { uid :: Int, name :: String}|]@
--
-- becomes
--
-- @data User t f = User { uid :: Wear t f Int, name :: Wear t f String }@
--
declareBareB :: DecsQ -> DecsQ
declareBareB decsQ = do
  decs <- decsQ
  decs' <- traverse go decs
  return $ concat decs'
  where
    go (DataD _ dataName tvbs _ [con@(RecC conName fields)] drv) = do
      varS <- newName "sw"
      varW <- newName "h"
      let xs = varNames "x" fields
      let ys = varNames "y" fields
      let transformed = transformCon varS varW con
      let names = foldl' AppE (ConE conName) [AppE (ConE 'Const) $ AppE (VarE 'fromString) $ LitE $ StringL $ nameBase name | (name, _, _) <- fields]
      let datC = conT dataName `appT` conT ''Covered
      decs <- [d|
        instance BareB $(conT dataName) where
          bcover $(conP conName $ map varP xs) = $(foldl'
              appE
              (conE conName)
              (appE (conE 'Identity) . varE <$> xs)
            )
          {-# INLINE bcover #-}
          bstrip $(conP conName $ map varP xs) = $(foldl'
              appE
              (conE conName)
              (appE (varE 'runIdentity) . varE <$> xs)
            )
          {-# INLINE bstrip #-}
        instance FieldNamesB $(datC) where bfieldNames = $(pure names)
        instance FunctorB $(datC) where
          bmap f $(conP conName $ map varP xs) = $(foldl'
              appE
              (conE conName)
              (appE (varE 'f) . varE <$> xs)
            )
        instance TraversableB $(datC) where
          btraverse f $(conP conName $ map varP xs) = $(fst $ foldl'
              (\(l, op) r -> (infixE (Just l) (varE op) (Just r), '(<*>)))
              (conE conName, '(<$>))
              (appE (varE 'f) . varE <$> xs)
            )
          {-# INLINE btraverse #-}
        instance ConstraintsB $(datC)
        instance ProductBC $(datC)
        instance ProductB $(datC) where
          bprod $(conP conName $ map varP xs) $(conP conName $ map varP ys) = $(foldl'
            (\r (x, y) -> [|$(r) (Pair $(varE x) $(varE y))|])
            (conE conName) (zip xs ys))
        |]
      return $ DataD [] dataName
        (tvbs ++ [PlainTV varS, PlainTV varW])
        Nothing
        [transformed]
        (DerivClause Nothing [ConT ''Generic] : drv)
        : decs
    go d = pure [d]

varNames :: String -> [VarBangType] -> [Name]
varNames p vbt = [mkName (p ++ nameBase v) | (v, _, _) <- vbt]

transformCon :: Name -- ^ switch variable
  -> Name -- ^ wrapper variable
  -> Con -- ^ original constructor
  -> Con
transformCon switchName wrapperName (RecC name xs) = RecC name
  [(v, b, ConT ''Wear
    `AppT` VarT switchName
    `AppT` VarT wrapperName
    `AppT` t)
  | (v, b, t) <- xs
  ]
transformCon var w (ForallC tvbs cxt con) = ForallC tvbs cxt $ transformCon var w con
transformCon _ _ con = error $ "transformCon: unsupported " ++ show con
