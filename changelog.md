# Change Log

## WIP

## [1.4.1.0] - 2020-05-04

- GHC 8.10 compatibility

## [1.4.0.0] - 2019-09-12

- Remove Generic instance, see
  https://github.com/expipiplus1/vector-sized/issues/79
- Add Binary instance
- Loosen bounds on dependencies

## [1.3.0.0] - 2019-05-28

- Correct type of accumulating fold functions

## [1.2.0.1] - 2019-05-22

- Loosen upper bounds on dependencies

## [1.2.0.0] - 2018-12-05

- Add ``Hashable`` instances
- Generalize ``concatMap``
- Various code and documentation cleanup
- Add ``SomeSized`` pattern

## [1.1.1.0] - 2018-11-13

- Fix build and add CI for 8.6.2

## [1.1.0.0] - 2018-11-08

- Generalise Ix instance
- Add Unboxed vectors
- Fiddle extension guarding

Thanks everyone!

## [1.0.5.0] - 2018-10-17

- Add Ix instance
- Add Comonad instance

Thanks mpilgrem and KingoftheHomeless

## [1.0.4.1] - 2018-09-04

- Fix compilation on ghc 8.6.

## [1.0.4.0] - 2018-07-14

- Add Monad instance for boxed vectors.

## [1.0.3.1] - 2018-07-10

- Loosen upper bound on `distributive`

## [1.0.3.0] - 2018-06-24

- Remove redundant KnownNat constraints

## [1.0.2.0] - 2018-05-15

- `not-home` haddock annotations for Internal modules, for more helpful linking
- Fix build with indexed-list-literals-0.2.1.0

## [1.0.1.0] - 2018-04-12

- Add Representable and Distributive instances for sized boxed vectors
- Use newer version of indexed-list-literals to allow a fully featured compile on ghc 8.4

## [1.0.0.0] - 2018-03-20

- More functions using `Finite` instead of `Int`
- Add `Read` and `Semigroup` instances
- Performance improvements for `Applicative`
- Add a `knownLength` function
- Add `fromTuple` (ghc < 8.3 for now)
- Add sized variants of mutable vectors 
- Expose sized vector constructors from Internal modules

Huge thanks to all the contributors!

## [0.6.1.0] - 2017-08-04
- Add lenses ix, _head and _last

## [0.6.0.0] - 2017-06-07
- Make ordering of additions in types be more consistent
- Make slice more general
- `Num`, `Fractional`, and `Floating` instances for vectors

## [0.5.1.0] - 2017-02-01
- Loosen upper bound on `vector`

## [0.5.0.0] - 2017-01-04
- Use `Finite` from `finite-typelits` for indexing.

## [0.4.1.0] - 2016-11-24
- Add `withSized` and `withSizedList`

## [0.4.0.1] - 2016-11-12
- Raise lower bound on base to 4.9

## [0.4.0.0] - 2016-11-01
- Correct type signature on `index'`

## [0.3.3.0] - 2016-08-10
- Add instances for Data, Typeable and Generic

## [0.3.2.0] - 2016-03-29
- Add overlapping Monoid instance for zero length vectors

## [0.3.1.0] - 2016-03-29
- Add Monoid instance

## [0.3.0.0] - 2016-03-22
- Export all of the available functionality from Data.Vector.Generic.
- Add Storable
- Add Unboxed

## [0.2.0.0] - 2016-02-29
- Tighter bounds on base to avoid compiling with GHC < 7.10.

## [0.1.0.0] - 2016-02-28
- Initial release.
