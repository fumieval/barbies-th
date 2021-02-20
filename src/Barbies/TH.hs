{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
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
{-# LANGUAGE PatternSynonyms #-}
module Barbies.TH (FieldNamesB(..)
  , LensB(..)
  , getLensB
  , AccessorsB(..)
  , declareBareB
  , declareBareBWithOtherBarbies
  ) where

import Language.Haskell.TH hiding (cxt)
import Language.Haskell.TH.Syntax (VarBangType, Name(..), mkOccName, occString)
import Data.String
import Data.Foldable (foldl')
import Data.List (partition, nub)
import Barbies
import Barbies.Constraints
import Barbies.Bare
import Data.Functor.Product
import GHC.Generics (Generic)
import Control.Applicative
import Data.Functor.Identity (Identity(..))
import Data.Functor.Compose (Compose(..))
import Data.List.Split
import Data.Maybe (mapMaybe)
import Data.Bool (bool)

-- | A pair of a getter and a setter
-- Not van Laarhoven to avoid dictionary passing
data LensB b a = LensB
  { viewB :: forall h. b h -> h a
  , setB :: forall h. h a -> b h -> b h
  }

nestLensB :: (forall h . a h -> (b h -> a h, b h)) -> LensB b c -> LensB a c
nestLensB l (LensB lv ls) =
  LensB (lv . snd . l) (\n h -> let (s, x) = l h in s (ls n x))

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
declareBareB = declareBareBWithOtherBarbies []

-- | Like 'declareBareB' except that one can specify the 'Name's of other
-- barbies. Members with these types won't be wrapped with 'Wear'.
declareBareBWithOtherBarbies :: [Name] -> DecsQ -> DecsQ
declareBareBWithOtherBarbies friends decsQ = do
  decs <- decsQ
  let newTypeNames = dataDecNames decs
  decs' <- traverse (go (newTypeNames <> friends)) decs
  return $ concat decs'
  where
    go otherBarbieNames (DataD _ dataName tvbs _ [con@(RecC nDataCon mangledfields)] classes) = do
      let fields = [(unmangle name, c, t) | (name, c, t) <- mangledfields]
      nSwitch <- newName "sw"
      nWrap <- newName "h"
      let xs = varNames "x" fields
      let ys = varNames "y" fields
      -- 'mapMembers' applies one of two functions to elements of a list
      -- according to whether or not they align with another barbie
      let otherBarbieMask = [ case t of
                                ConT n | n `elem` otherBarbieNames -> True
                                _ -> False
                            | (_, _, t) <- fields
                            ]
      let mapMembers :: (b -> c) -> (b -> c) -> [b] -> [c]
          mapMembers normal otherBarbie = zipWith (bool normal otherBarbie) otherBarbieMask
      nData <- newName "b"
      nConstr <- newName "c"
      nX <- newName "x"
      let transformed = transformCon otherBarbieNames nSwitch nWrap con
      let reconE = foldl' appE (conE nDataCon)
          -- field names for FieldNamesB
          fieldNamesE = reconE $ mapMembers
            (\(name,_,_) -> [|Const $ fromString $(litE $ StringL $ nameBase name)|])
            (const [|bfieldNames|])
            fields
          accessors = reconE $ mapMembers
            (\name -> [|LensB
                $(varE name)
                (\ $(varP nX) $(varP nData) -> $(recUpdE (varE nData) [pure (name, VarE nX)])) |]
            )
            (\name -> [|bmap
                          (nestLensB
                             (\ $(varP nData) -> (\ $(varP nX) -> $(recUpdE (varE nData) [pure (name, VarE nX)])
                                                 ,$(varE name) $(varE nData)
                                                 )
                             )
                          )
                          baccessors
                      |]
            )
            [name | (name,_,_) <- fields]


          -- Turn TyVarBndr into just a Name such that we can
          -- reconstruct the constructor applied to already-present
          -- type variables below.
#if MIN_VERSION_template_haskell(2,17,0)
          varName (PlainTV n _) = n
          varName (KindedTV n _ _) = n
#else
          varName (PlainTV n) = n
          varName (KindedTV n _) = n
#endif

          -- The type name as present originally along with its type
          -- variables.
          vanillaType = foldl' appT (conT dataName) (varT . varName <$> tvbs)

          -- max arity = 62
          typeChunks = chunksOf 62 (mapMembers
              (\t -> varT nConstr `appT` t)
              (\t -> [t| AllB $(varT nConstr) ($t Covered)|])
              [pure t | (_, _, t) <- fields]
            )
          mkConstraints ps = foldl appT (tupleT $ length ps) ps
          allConstr = case typeChunks of
            [ps] -> mkConstraints ps
            pss -> mkConstraints $ map mkConstraints pss

      let datC = vanillaType `appT` conT ''Covered
      decs <- [d|
        instance BareB $(vanillaType) where
          bcover $(conP nDataCon $ map varP xs)
            = $(reconE $ mapMembers (appE (conE 'Identity)) (appE (varE 'bcover)) (varE <$> xs))
          {-# INLINE bcover #-}
          bstrip $(conP nDataCon $ map varP xs)
            = $(reconE $ mapMembers (appE (varE 'runIdentity)) (appE (varE 'bstrip)) (varE <$> xs))
          {-# INLINE bstrip #-}
        instance FieldNamesB $(datC) where bfieldNames = $(fieldNamesE)
        instance AccessorsB $(datC) where baccessors = $(accessors)
        instance FunctorB $(datC) where
          bmap f $(conP nDataCon $ map varP xs)
            = $(reconE (mapMembers (appE (varE 'f)) (appE [|bmap f|]) (varE <$> xs)))
        instance DistributiveB $(datC) where
          bdistribute fb = $(reconE $
              -- TODO: NoFieldSelectors
              mapMembers
                (\fd -> [| Compose ($fd <$> fb) |])
                (\fd -> [| bdistribute ($fd <$> fb) |])
                [varE fd | (fd, _, _) <- fields]
            )
        instance TraversableB $(datC) where
          btraverse f $(conP nDataCon $ map varP xs) = $(fst $ foldl'
              (\(l, op) r -> (infixE (Just l) (varE op) (Just r), '(<*>)))
              (conE nDataCon, '(<$>))
              (mapMembers (appE (varE 'f)) (\x -> [|btraverse f $x|]) (varE <$> xs))
            )
          {-# INLINE btraverse #-}
        instance ConstraintsB $(datC) where
          type AllB $(varT nConstr) $(datC) = $(allConstr)
          baddDicts $(conP nDataCon $ map varP xs)
            = $(reconE $ mapMembers
                 (\x -> [|Pair Dict $x|])
                 (\x -> [|baddDicts $x|])
                 (varE <$> xs)
               )
        instance ApplicativeB $(datC) where
          bpure $(varP nX) = $(reconE $ mapMembers
                                 (const (varE nX))
                                 (const [|bpure $(varE nX)|])
                                 xs
                              )
          bprod $(conP nDataCon $ map varP xs) $(conP nDataCon $ map varP ys) = $(foldl'
            (\r (isOtherBarbie, x, y) ->
              if isOtherBarbie
                then [|$r (bprod $(varE x) $(varE y))|]
                else [|$r (Pair $(varE x) $(varE y))|])
            (conE nDataCon) (zip3 otherBarbieMask xs ys))
        |]
      -- strip deriving Generic
      let classes' = map (\(DerivClause strat cs) -> fmap (DerivClause strat) $ partition (== ConT ''Generic) cs) classes
      -- For the covered type, derive instances via 'Barbie' wrapper instead.
      coverDrvs <- traverse (\cls ->
        [d|deriving via Barbie $(datC) $(varT nWrap)
            instance ($(cls) (Barbie $(datC) $(varT nWrap))) => $(cls) ($(datC) $(varT nWrap))|])
        [ pure t | (_, DerivClause _ preds) <- classes', t <- preds ]
      -- Redefine instances of the bare type with the original strategy
      bareDrvs <- traverse (\(strat, cls) ->
        standaloneDerivWithStrategyD strat (pure []) [t|$(cls) ($(vanillaType) Bare Identity)|])
        [ (strat, pure t) | (_, DerivClause strat preds) <- classes', t <- preds ]
      return $ DataD [] dataName
#if MIN_VERSION_template_haskell(2,17,0)
        (tvbs ++ [PlainTV nSwitch (), PlainTV nWrap ()])
#else
        (tvbs ++ [PlainTV nSwitch, PlainTV nWrap])
#endif
        Nothing
        [transformed]
        [DerivClause Nothing $ concatMap fst classes']
        : decs ++ concat coverDrvs ++ bareDrvs
    go _ d = pure [d]

dataDecNames :: [Dec] -> [Name]
dataDecNames = nub . mapMaybe decName
 where
  decName :: Dec -> Maybe Name
  decName = \case
    DataD    _ n _ _ _ _ -> Just n
    _                    -> Nothing

varNames :: String -> [VarBangType] -> [Name]
varNames p vbt = [mkName (p ++ nameBase v) | (v, _, _) <- vbt]

transformCon :: [Name] -- ^ Names of other barbies
  -> Name -- ^ switch variable
  -> Name -- ^ wrapper variable
  -> Con -- ^ original constructor
  -> Con
transformCon otherBarbieNames switchName wrapperName (RecC name xs) = RecC
  name
  [ (unmangle v, b, t')
  | (v, b, t) <- xs
  , let
    t' = case t of
      ConT n | n `elem` otherBarbieNames ->
        ConT n `AppT` VarT switchName `AppT` VarT wrapperName
      _ -> ConT ''Wear `AppT` VarT switchName `AppT` VarT wrapperName `AppT` t
  ]
transformCon otherBarbieNames var w (ForallC tvbs cxt con) =
  ForallC tvbs cxt $ transformCon otherBarbieNames var w con
transformCon _ _ _ con = error $ "transformCon: unsupported " ++ show con

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
