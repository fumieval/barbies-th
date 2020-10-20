{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
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
    }|]
main = pure ()
