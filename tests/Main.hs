{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS -ddump-splices #-}
module Main where
import Data.Barbie.TH
import GHC.Generics
import Barbies
import Barbies.Bare
declareBareB [d|
  data Foo = Foo
    { foo :: Int
    , bar :: String
    }  |]
main = pure ()
