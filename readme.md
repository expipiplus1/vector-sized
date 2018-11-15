# ``vector-sized`` [![Hackage][hackage-shield]][hackage]

This package exports a newtype tagging the vectors from the [``vector``][1]
package with a type-level natural representing their sized. It also exports
functions from ``vector`` whose size can be determined ahead of time,
appropriately retyped.

Currently, we provide size-tagged versions of the following:

* [``Data.Vector.Vector``][2], in ``Data.Vector.Sized``
* [``Data.Vector.Generic.Vector``][5], in ``Data.Vector.Generic.Sized``
* [``Data.Vector.Storable.Vector``][3], in ``Data.Vector.Storable.Sized``
* [``Data.Vector.Unboxed.Vector``][4], in ``Data.Vector.Unboxed.Sized``

We also provide mutable versions of each of the above. Additionally, we include
functions for converting to and from 'unsized' vectors and lists, using
CPS-style existentials.

The code in this package is based on the initial work by Ben Gamari in a [PR for
``vulkan``][7].

## How is this different to ``fixed-vector``?

This package is fairly similar to [``fixed-vector``][6], as both libraries are
designed to provide vectors of statically known length. However, the
implementations used are different, with different tradeoffs. ``vector-sized``
uses a newtype wrapper around vectors from ``vector``, and is thus able to
handle vectors of arbitrary length. However, this approach requires us to carry
a runtime representation of length, which is a significant memory overhead for
small vectors. ``fixed-vector`` instead defines all functions as manipulations
of Church-encoded product types of the form ``∀r. (a → a → r) → r`` (for 2D
vectors), allowing it to work for both arbitrary product types (like ``data V2 a
= V2 a a``) and opaque length-parameterized vectors. However, as a consequence
of this implementation choice, ``fixed-vector`` cannot handle vectors whose size
exceeds tens of elements.

[1]: https://hackage.haskell.org/package/vector
[2]: https://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector.html#t:Vector
[3]: https://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector-Storable.html#t:Vector
[4]: https://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector-Unboxed.html#t:Vector
[5]: https://hackage.haskell.org/package/vector-0.12.0.1/docs/Data-Vector-Generic.html#t:Vector  
[6]: https://hackage.haskell.org/package/fixed-vector
[7]: https://github.com/expipiplus1/vulkan/pull/1
[hackage-shield]: https://img.shields.io/badge/hackage-v1.1.10-blue.svg
[hackage]: http://hackage.haskell.org/package/vector-sized
