barbies-th
====

[![Hackage](https://img.shields.io/hackage/v/barbies-th.svg)](https://hackage.haskell.org/package/barbies-th)
![Haskell CI](https://github.com/fumieval/barbies-th/workflows/Haskell%20CI/badge.svg)

A wrapper library for [barbies](http://hackage.haskell.org/package/barbies) to generate [strippable HKD](http://hackage.haskell.org/package/barbies-1.1.3.0/docs/Data-Barbie-Bare.html)s. It transforms the following declaration

```haskell
declareBareB [d|
  data Foo = Foo
    { foo :: Int
    , bar :: String
    }  |]
```

into:

```haskell
data Foo sw h = Foo
    { foo :: Wear sw h Int,
    , bar :: Wear sw h String
    }
instance BareB Foo
instance FieldNamesB (Foo Covered) where
  bfieldNames = Foo (Const "foo") (Const "bar")
instance ProductB (Foo Covered) where
  bprod (Foo xfoo xbar) (Foo yfoo ybar)
    = Foo (Pair xfoo yfoo) (Pair xbar ybar)
instance FunctorB (Foo Covered) where ...
instance TraversableB (Foo Covered) where ...
instance ConstraintsB (Foo Covered)
instance ProductBC (Foo Covered)
```

Typically you need the following extensions to make `declareBareB` work:

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
```

GHC sometimes takes very long time to compile code with generically derived instances, and it often fails to inline functions properly too. This package generates most instance methods by TH, reducing large amount of compilation time
of the declarations and use sites.

Unlike [higgledy](https://hackage.haskell.org/package/higgledy) which relies on
in-memory representation using `GHC.Generic`, you don't have to worry about the performance, and you can benefit from various language features
(e.g. -Wmissing-fields, `RecordWildCards` etc) even in higher-kinded form.

Deriving pass-through
----

stock deriving does not work on HKDs. Instead, it transforms deriving clauses into standalone ones via the `Barbie` wrapper,
as well as ones for the `Bare` counterpart. For example,

```haskell
data Foo = ... deriving (Show, Eq)
```

generates

```haskell
deriving instance Show (Foo Bare Identity)
deriving instance Eq (Foo Bare Identity)
deriving via Barbie (Foo Covered) h instance Show (Barbie (Foo Covered) h) => Show (Foo Covered h)
deriving via Barbie (Foo Covered) h instance Eq (Barbie (Foo Covered) h) => Eq (Foo Covered h)
```

Note that `Barbies` module must be imported manually.

Matryoshka barbies
----

Barbies can contain other barbies if they're declared in the same splice, it
pretty much works as you'd expect.

```hasklell
declareBareB [d|
  data Inner = Inner
    { inner :: Int
    }
  data Outer = Outer
    { outer :: Inner
    , other :: Bool
    }
|]
```

into:

```haskell
data Inner sw h = Inner
    { inner :: Wear sw h Int
    }
data Outer sw h = Outer
    { outer :: Inner sw h
    , other :: Wear sw h Bool
    }
-- And all the instances as above
```
