# Revision history for barbies-th

## 0.1.8

* `declareBareB` can now generate nested barbies when multiple data declarations are passed (#7)
* Fixed a bug breaking `AccessorsB` instances when `DuplicateRecordFields` is enabled (#6)

Kudos to Gergő Érdi(@gergoerdi) and Joe Hermaszewski(@expipiplus1)

## 0.1.7

* `declareBareB` now generates deriving clauses for the bare type too
* It no longer adds `deriving Generic` unconditionally

## 0.1.6

* Added a workaround to prevent exceeding the max arity of constraint tuples

## 0.1.5

* It now generates `ConstraintsB` and `ApplicativeB` declarations without generics

## 0.1.4

* Renamed `Data.Barbie.TH` to `Barbies.TH`, leaving `Data.Barbie.TH` deprecated

## 0.1.3

* Added `LensB` and `getLensB`
* Now derives `AccessorsB`

## 0.1.2

* `declareBareB` now derives `DistributiveD`

## 0.1.1 -- 2020/04/19

* Improved the deriving mechanism; deriving clauses within `declareBareB` are translated into `deriving via Barbie ...`

## 0.1 -- 2020/02

* First version. Released on an unsuspecting world.
