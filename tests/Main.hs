{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
-- Not required, but it shouldn't break things.
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS -ddump-splices #-}
module Main where
import Barbies.TH
import GHC.Generics
import Barbies
import Barbies.Bare

declareBareB [d|
  data Foo = Foo
    { foo :: Int
    , bar :: String
    } deriving (Show, Eq, Generic)|]

-- Nested
declareBareB [d|
  data Inner = Inner
    { inner :: Int
    } deriving (Show, Eq, Generic)
  data Outer = Outer
    { outer :: Inner
    , other :: Bool
    } deriving (Show, Eq, Generic)
    |]

declareBareBWithOtherBarbies [''Foo] [d|
  data Baz = Baz
    { baz :: Foo
    }
    |]

main = pure ()
