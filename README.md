barbies-th
====

A wrapper library for barbies to generate strippable HKDs. The following declaration

```haskell
declareBareB [d|
  data Foo = Foo
    { foo :: Int
    , bar :: String
    }  |]
```

creates:

```haskell
data Foo sw h = Foo
    { foo :: Wear sw h Int,
    , bar :: Wear sw h String
    } deriving Generic
instance BareB Foo
instance FieldNamesB (Foo Covered) where
  bfieldNames = Foo (Const "foo") (Const "bar")
instance ProductB (Foo Covered) where
  bprod (Foo xfoo xbar) (Foo yfoo ybar)
    = Foo (Pair xfoo yfoo) (Pair xbar ybar)
instance FunctorB (Foo Covered)
instance TraversableB (Foo Covered)
instance ConstraintsB (Foo Covered)
instance ProductBC (Foo Covered)
```
