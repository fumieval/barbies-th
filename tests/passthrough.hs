{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -ddump-splices #-}
module Main where
import Barbies.TH
import GHC.Generics
import Barbies
import Barbies.Bare
import Data.Functor.Identity

passthroughBareB [d|
  data Foo = Foo
    { foo :: Int
    , bar :: String
    } deriving (Show, Eq, Generic)|]

passthroughBareB [d|
  data Inner = Inner
    { inner :: Int
    } deriving (Show, Eq, Generic)
  data Outer = Outer
    { outer :: Inner
    , other :: Bool
    } deriving (Show, Eq, Generic)
    |]

test_con :: FooH []
test_con = Foo
  { foo = [0]
  , bar = ["Haskell"]
  }

test_bare :: Inner
test_bare = Inner 0 :: InnerB Bare Identity

test_sel :: ([Int], [String])
test_sel = (foo test_con, bar test_con)

test_upd :: FooH []
test_upd = test_con { foo = [], bar = [] }

main = pure ()

