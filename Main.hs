{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS -ddump-splices #-}
module Main where
import Data.Barbies.TH
import GHC.Generics
import Data.Barbie
import Data.Barbie.Bare
declareBareB [d|
  data Foo = Foo
    { foo :: Int
    , bar :: String
    }  |]
