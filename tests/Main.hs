{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
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

test_con :: Foo Covered []
test_con = Foo
  { foo = [0]
  , bar = ["Haskell"]
  }

test_sel :: ([Int], [String])
test_sel = (foo test_con, bar test_con)

test_upd :: Foo Covered []
test_upd = test_con { foo = [], bar = [] }

main = pure ()