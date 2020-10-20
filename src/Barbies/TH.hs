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
module Barbies.TH (FieldNamesB(..)
  , LensB(..)
  , getLensB
  , AccessorsB(..)
  , declareBareB
  ) where

import Language.Haskell.TH hiding (cxt)
import Language.Haskell.TH.Syntax (VarBangType, Name(..), mkOccName, occString)
import Data.String
import Data.Foldable (foldl')
import Data.List (partition)
import Barbies
import Barbies.Constraints
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
    go (DataD _ dataName tvbs _ [con@(RecC nDataCon fields)] classes) = do
      nSwitch <- newName "sw"
      nWrap <- newName "h"
      let xs = varNames "x" fields
      let ys = varNames "y" fields
      nData <- newName "b"
      nConstr <- newName "c"
      nX <- newName "x"
      let transformed = transformCon nSwitch nWrap con
      let reconE = foldl' appE (conE nDataCon)
          -- field names for FieldNamesB
          fieldNamesE = reconE [[|Const $ fromString $(litE $ StringL $ nameBase name)|] | (name, _, _) <- fields]
          accessors = reconE
            [ [|LensB
                $(varE name)
                (\ $(varP nX) $(varP nData) -> $(recUpdE (varE nData) [pure (name, VarE nX)])) |]
            | (name, _, _) <- fields]

          -- Turn TyVarBndr into just a Name such that we can
          -- reconstruct the constructor applied to already-present
          -- type variables below.
          varName (PlainTV n) = n
          varName (KindedTV n _) = n

          -- The type name as present originally along with its type
          -- variables.
          vanillaType = foldl' appT (conT dataName) (varT . varName <$> tvbs)

          -- max arity = 62
          typeChunks = chunksOf 62 [varT nConstr `appT` pure t | (_, _, t) <- fields]
          mkConstraints ps = foldl appT (tupleT $ length ps) ps
          allConstr = case typeChunks of
            [ps] -> mkConstraints ps
            pss -> mkConstraints $ map mkConstraints pss

      let datC = vanillaType `appT` conT ''Covered
      decs <- [d|
        instance BareB $(vanillaType) where
          bcover $(conP nDataCon $ map varP xs)
            = $(reconE $ appE (conE 'Identity) . varE <$> xs)
          {-# INLINE bcover #-}
          bstrip $(conP nDataCon $ map varP xs)
            = $(reconE $ appE (varE 'runIdentity) . varE <$> xs)
          {-# INLINE bstrip #-}
        instance FieldNamesB $(datC) where bfieldNames = $(fieldNamesE)
        instance AccessorsB $(datC) where baccessors = $(accessors)
        instance FunctorB $(datC) where
          bmap f $(conP nDataCon $ map varP xs)
            = $(reconE (appE (varE 'f) . varE <$> xs))
        instance DistributiveB $(datC) where
          bdistribute fb = $(reconE
              -- TODO: NoFieldSelectors
              [ [| Compose ($(varE (unmangle fd)) <$> fb) |] | (fd, _, _) <- fields ]
            )
        instance TraversableB $(datC) where
          btraverse f $(conP nDataCon $ map varP xs) = $(fst $ foldl'
              (\(l, op) r -> (infixE (Just l) (varE op) (Just r), '(<*>)))
              (conE nDataCon, '(<$>))
              (appE (varE 'f) . varE <$> xs)
            )
          {-# INLINE btraverse #-}
        instance ConstraintsB $(datC) where
          type AllB $(varT nConstr) $(datC) = $(allConstr)
          baddDicts $(conP nDataCon $ map varP xs)
            = $(reconE $ map (\x -> [|Pair Dict $(varE x)|]) xs)
        instance ApplicativeB $(datC) where
          bpure $(varP nX) = $(reconE $ varE nX <$ xs)
          bprod $(conP nDataCon $ map varP xs) $(conP nDataCon $ map varP ys) = $(foldl'
            (\r (x, y) -> [|$(r) (Pair $(varE x) $(varE y))|])
            (conE nDataCon) (zip xs ys))
        |]
      let classes' = map (\(DerivClause _ cs) -> partition (== ConT ''Generic) cs) classes
      -- Derive instances via 'Barbie' wrapper instead.
      drvs <- traverse (\cls ->
        [d|deriving via Barbie $(datC) $(varT nWrap)
            instance ($(cls) (Barbie $(datC) $(varT nWrap))) => $(cls) ($(datC) $(varT nWrap))|])
        [ pure t | (_, preds) <- classes', t <- preds ]
      return $ DataD [] dataName
        (tvbs ++ [PlainTV nSwitch, PlainTV nWrap])
        Nothing
        [transformed]
        [DerivClause Nothing $ concatMap fst classes']
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
