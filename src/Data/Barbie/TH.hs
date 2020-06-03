{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Barbie.TH (FieldNamesB(..)
  , LensB(..)
  , getLensB
  , AccessorsB(..)
  , declareBareB
  ) where

import Language.Haskell.TH hiding (cxt)
import Language.Haskell.TH.Syntax (VarBangType, Name(..), mkOccName, occString)
import Data.String
import Data.Foldable (foldl')
import Barbies
import Barbies.Bare
import Data.Functor.Product
import GHC.Generics (Generic)
import Control.Applicative
import Data.Functor.Identity (Identity(..))
import Data.Functor.Compose (Compose(..))
import Data.List.Split

-- | A pair of a getter and a setter
-- Not van Laarhoven to avoid dictionary passing
data LensB b a = LensB
  { viewB :: forall h. b h -> h a
  , setB :: forall h. h a -> b h -> b h
  }

getLensB :: Functor f => LensB b a -> (h a -> f (h a)) -> b h -> f (b h)
getLensB (LensB v s) f b = (\x -> s x b) <$> f (v b)
{-# INLINE getLensB #-}

class AccessorsB b where
  -- | A collection of lenses (getter-setter pairs)
  baccessors :: b (LensB b)

-- | barbies doesn't care about field names, but they are useful in many use cases
class FieldNamesB b where
  -- | A collection of field names.
  bfieldNames :: IsString a => b (Const a)

-- | Transform a regular Haskell record declaration into HKD form.
-- 'BareB', 'FieldNamesB', 'FunctorB', 'DistributiveB',
-- 'TraversableB', 'ApplicativeB' and 'ConstraintsB' instances are
-- derived.
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
    go (DataD _ dataName tvbs _ [con@(RecC conName fields)] classes) = do
      varS <- newName "sw"
      varW <- newName "h"
      let xs = varNames "x" fields
      let ys = varNames "y" fields
      varB <- newName "b"
      let transformed = transformCon varS varW con
      let names = foldl' AppE (ConE conName) [AppE (ConE 'Const) $ AppE (VarE 'fromString) $ LitE $ StringL $ nameBase name | (name, _, _) <- fields]
          accessors = foldl' appE (conE conName)
            [ [|LensB
                $(varE name)
                (\ $(varP varW) $(varP varB) -> $(recUpdE (varE varB) [pure (name, VarE varW)])) |]
            | (name, _, _) <- fields]

          -- Turn TyVarBndr into just a Name such that we can
          -- reconstruct the constructor applied to already-present
          -- type variables below.
          varName (PlainTV n) = n
          varName (KindedTV n _) = n

          -- The type name as present originally along with its type
          -- variables.
          vanillaType = foldl' appT (conT dataName) (varT . varName <$> tvbs)

      let datC = vanillaType `appT` conT ''Covered
      decs <- [d|
        instance BareB $(vanillaType) where
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
        instance AccessorsB $(datC) where baccessors = $(accessors)
        instance FunctorB $(datC) where
          bmap f $(conP conName $ map varP xs) = $(foldl'
              appE
              (conE conName)
              (appE (varE 'f) . varE <$> xs)
            )
        instance DistributiveB $(datC) where
          bdistribute fb = $(foldl'
              appE
              (conE conName)
              [ [| Compose ($(varE (unmangle fd)) <$> fb) |] | (fd, _, _) <- fields ]
            )
        instance TraversableB $(datC) where
          btraverse f $(conP conName $ map varP xs) = $(fst $ foldl'
              (\(l, op) r -> (infixE (Just l) (varE op) (Just r), '(<*>)))
              (conE conName, '(<$>))
              (appE (varE 'f) . varE <$> xs)
            )
          {-# INLINE btraverse #-}
        instance ConstraintsB $(datC)
        instance ApplicativeB $(datC) where
          bprod $(conP conName $ map varP xs) $(conP conName $ map varP ys) = $(foldl'
            (\r (x, y) -> [|$(r) (Pair $(varE x) $(varE y))|])
            (conE conName) (zip xs ys))
        |]
      drvs <- traverse (\cls ->
        [d|deriving via Barbie $(datC) $(varT varW)
            instance ($(cls) (Barbie $(datC) $(varT varW))) => $(cls) ($(datC) $(varT varW))|])
        [ pure t | DerivClause _ preds <- classes, t <- preds ]
      return $ DataD [] dataName
        (tvbs ++ [PlainTV varS, PlainTV varW])
        Nothing
        [transformed]
        [DerivClause Nothing [ConT ''Generic]]
        : decs ++ concat drvs
    go d = pure [d]

varNames :: String -> [VarBangType] -> [Name]
varNames p vbt = [mkName (p ++ nameBase (unmangle v)) | (v, _, _) <- vbt]

transformCon :: Name -- ^ switch variable
  -> Name -- ^ wrapper variable
  -> Con -- ^ original constructor
  -> Con
transformCon switchName wrapperName (RecC name xs) = RecC name
  [(unmangle v, b, ConT ''Wear
    `AppT` VarT switchName
    `AppT` VarT wrapperName
    `AppT` t)
  | (v, b, t) <- xs
  ]
transformCon var w (ForallC tvbs cxt con) = ForallC tvbs cxt $ transformCon var w con
transformCon _ _ con = error $ "transformCon: unsupported " ++ show con

-- | Unmangle record field names
--
-- When 'DuplicateRecordFields' is turned on, record field names are mangled.
-- (see https://gitlab.haskell.org/ghc/ghc/-/wikis/records/overloaded-record-fields/duplicate-record-fields#mangling-selector-names)
-- We undo that because these mangled field names don't round-trip through TH splices.
unmangle :: Name -> Name
unmangle (Name occ flavour) = Name occ' flavour
  where
    occ' = case wordsBy (== ':') (occString occ) of
        ["$sel", fd, _qual] -> mkOccName fd
        _ -> occ
