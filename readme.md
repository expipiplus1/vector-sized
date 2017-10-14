# Vector Sized

This package exports a newtype tagging the vectors from the
[vector](https://hackage.haskell.org/package/vector) package with a type level
natural representing their size.
It also exports a few functions from vector appropriately retyped.

This package is fairly similar to
the [fixed-vector](https://hackage.haskell.org/package/fixed-vector) package.
While both provide vectors of statically know length they use completely
different implementation with different tradeoffs. `vector-sized` is a newtype
wrapper over `vector` thus it's able to handle vectors of arbitrary length but
have to carry runtime representation of length which is significant memory
overhead for small vectors. `fixed-vector` defines all functions as
manipulations of Church-encoded product types (`∀r. (a→a→r) → r` for 2D vectors)
so it can work for both arbitrary product types like `data V2 a = V2 a a` and
opaque length-parametrized vectors provided by library. As consequence of
implementation it can't handle vectors larger than tens of elements.


The initial code for this package was written by @bgamari in a [PR for vulkan](https://github.com/expipiplus1/vulkan/pull/1)
