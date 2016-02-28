# Vector Sized

This package exports a newtype tagging the vectors from the
[vector](https://hackage.haskell.org/package/vector) package with a type level
natural representing their size.

It also exports a few functions from vector appropriately retyped.

This package is fairly similar to the
[fixed-vector](https://hackage.haskell.org/package/fixed-vector) package. The
difference is that fixed-vector uses Peano naturals to represent the size tag
on the vectors and this package uses typelits.

