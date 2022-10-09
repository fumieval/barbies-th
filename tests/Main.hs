{-# LANGUAGE FlexibleContexts #-}
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

declareBareB [d|
  data EmptyRecord = EmptyRecord {}
    |]

test_con :: Foo Covered []
test_con = Foo
  { foo = [0]
  , bar = ["Haskell"]
  }

test_sel :: ([Int], [String])
test_sel = (foo test_con, bar test_con)

test_upd :: Foo Covered []
test_upd = test_con { foo = [], bar = [] }

test_empty :: (EmptyRecord Covered [], EmptyRecord Covered Maybe)
test_empty = (EmptyRecord, EmptyRecord)

main = pure ()

