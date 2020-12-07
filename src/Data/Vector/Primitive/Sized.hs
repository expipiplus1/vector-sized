{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE CPP                 #-}

#if MIN_VERSION_base(4,12,0)
{-# LANGUAGE NoStarIsType #-}
#endif

{-|
This module re-exports the functionality in 'Data.Vector.Generic.Sized'
 specialized to 'Data.Vector.Primitive'.

Functions returning a vector determine the size from the type context unless
they have a @'@ suffix in which case they take an explicit 'Proxy' argument.

Functions where the resulting vector size is not known until runtime are
not exported.
-}

module Data.Vector.Primitive.Sized
 ( Vector
  , pattern SomeSized
  , VSM.MVector
   -- * Accessors
   -- ** Length information
  , length
  , length'
  , knownLength
  , knownLength'
    -- ** Indexing
  , index
  , index'
  , unsafeIndex
  , head
  , last
    -- ** Monadic indexing
  , indexM
  , indexM'
  , unsafeIndexM
  , headM
  , lastM
    -- ** Extracting subvectors (slicing)
  , slice
  , slice'
  , init
  , tail
  , take
  , take'
  , drop
  , drop'
  , splitAt
  , splitAt'
    -- * Construction
    -- ** Initialization
  , empty
  , singleton
  , fromTuple
  , replicate
  , replicate'
  , generate
  , generate'
  , iterateN
  , iterateN'
    -- ** Monadic initialization
  , replicateM
  , replicateM'
  , generateM
  , generateM'
    -- ** Unfolding
  , unfoldrN
  , unfoldrN'
    -- ** Enumeration
  , enumFromN
  , enumFromN'
  , enumFromStepN
  , enumFromStepN'
    -- ** Concatenation
  , cons
  , snoc
  , (++)
    -- ** Restricting memory usage
  , force
    -- * Modifying vectors
    -- ** Bulk updates
  , (//)
  , update_
  , unsafeUpd
  , unsafeUpdate_
    -- ** Accumulations
  , accum
  , accumulate_
  , unsafeAccum
  , unsafeAccumulate_
    -- ** Permutations
  , reverse
  , backpermute
  , unsafeBackpermute
    -- * Lenses
  , ix
  , _head
  , _last
    -- * Elementwise operations
    -- ** Mapping
  , map
  , imap
  , concatMap
    -- ** Monadic mapping
  , mapM
  , imapM
  , mapM_
  , imapM_
  , forM
  , forM_
    -- ** Zipping
  , zipWith
  , zipWith3
  , zipWith4
  , zipWith5
  , zipWith6
  , izipWith
  , izipWith3
  , izipWith4
  , izipWith5
  , izipWith6
    -- ** Monadic zipping
  , zipWithM
  , izipWithM
  , zipWithM_
  , izipWithM_
    -- * Working with predicates
    -- ** Searching
  , elem
  , notElem
  , find
  , findIndex
  , elemIndex
    -- * Folding
  , foldl
  , foldl1
  , foldl'
  , foldl1'
  , foldr
  , foldr1
  , foldr'
  , foldr1'
  , ifoldl
  , ifoldl'
  , ifoldr
  , ifoldr'
    -- ** Specialised folds
  , all
  , any
  , sum
  , product
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  , maxIndex
  , maxIndexBy
  , minIndex
  , minIndexBy
    -- ** Monadic folds
  , foldM
  , ifoldM
  , fold1M
  , foldM'
  , ifoldM'
  , fold1M'
  , foldM_
  , ifoldM_
  , fold1M_
  , foldM'_
  , ifoldM'_
  , fold1M'_
    -- * Prefix sums (scans)
  , prescanl
  , prescanl'
  , postscanl
  , postscanl'
  , scanl
  , scanl'
  , scanl1
  , scanl1'
  , prescanr
  , prescanr'
  , postscanr
  , postscanr'
  , scanr
  , scanr'
  , scanr1
  , scanr1'
    -- * Conversions
    -- ** Lists
  , toList
  , fromList
  , fromListN
  , fromListN'
  , withSizedList
    -- ** Mutable vectors
  , freeze
  , thaw
  , copy
  , unsafeFreeze
  , unsafeThaw
    -- ** Unsized Vectors
  , toSized
  , withSized
  , fromSized
  , withVectorUnsafe
  ) where

import qualified Data.Vector.Generic.Sized as V
import qualified Data.Vector.Primitive as VS
import Data.IndexedListLiterals (IndexedListLiterals)
import qualified Data.Vector.Primitive.Mutable.Sized as VSM
import GHC.TypeLits
import Data.Finite
import Data.Primitive (Prim)
import Data.Proxy
import Control.Monad.Primitive
import Prelude hiding ( length, null,
                        replicate, (++), concat,
                        head, last,
                        init, tail, take, drop, splitAt, reverse,
                        map, concat, concatMap,
                        zipWith, zipWith3, zip, zip3, unzip, unzip3,
                        filter, takeWhile, dropWhile, span, break,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1,
                        all, any, and, or, sum, product, maximum, minimum,
                        scanl, scanl1, scanr, scanr1,
                        enumFromTo, enumFromThenTo,
                        mapM, mapM_, sequence, sequence_,
                        showsPrec )

-- | 'Data.Vector.Generic.Sized.Vector' specialized to use
-- 'Data.Vector.Primitive'.
type Vector = V.Vector VS.Vector

-- | /O(1)/ Yield the length of the vector as an 'Int'. This is more like
-- 'natVal' than 'Data.Vector.length', extracting the value from the 'KnownNat'
-- instance and not looking at the vector itself.
length :: forall n a. KnownNat n
       => Vector n a -> Int
length = V.length
{-# inline length #-}

-- | /O(1)/ Yield the length of the vector as a 'Proxy'. This function
-- doesn't /do/ anything; it merely allows the size parameter of the vector
-- to be passed around as a 'Proxy'.
length' :: forall n a.
           Vector n a -> Proxy n
length' = V.length'
{-# inline length' #-}

-- | /O(1)/ Reveal a 'KnownNat' instance for a vector's length, determined
-- at runtime.
knownLength :: forall n a r. Prim a
            => Vector n a -- ^ a vector of some (potentially unknown) length
            -> (KnownNat n => r) -- ^ a value that depends on knowing the vector's length
            -> r -- ^ the value computed with the length
knownLength = V.knownLength

-- | /O(1)/ Reveal a 'KnownNat' instance and 'Proxy' for a vector's length,
-- determined at runtime.
knownLength' :: forall n a r. Prim a
             => Vector n a -- ^ a vector of some (potentially unknown) length
             -> (KnownNat n => Proxy n -> r) -- ^ a value that depends on knowing the vector's length, which is given as a 'Proxy'
             -> r -- ^ the value computed with the length
knownLength' = V.knownLength'

-- | /O(1)/ Safe indexing using a 'Finite'.
index :: forall n a. Prim a
      => Vector n a -> Finite n -> a
index = V.index
{-# inline index #-}

-- | /O(1)/ Safe indexing using a 'Proxy'.
index' :: forall n m a p. (KnownNat n, Prim a)
       => Vector (n+m+1) a -> p n -> a
index' = V.index'
{-# inline index' #-}

-- | /O(1)/ Indexing using an 'Int' without bounds checking.
unsafeIndex :: forall n a. Prim a
      => Vector n a -> Int -> a
unsafeIndex = V.unsafeIndex
{-# inline unsafeIndex #-}

-- | /O(1)/ Yield the first element of a non-empty vector.
head :: forall n a. (Prim a)
     => Vector (1+n) a -> a
head = V.head
{-# inline head #-}

-- | /O(1)/ Yield the last element of a non-empty vector.
last :: forall n a. (Prim a)
     => Vector (n+1) a -> a
last = V.last
{-# inline last #-}

-- | Lens to access (/O(1)/) and update (/O(n)/) an arbitrary element by its index.
ix :: forall n a f. (Prim a, Functor f)
   => Finite n -> (a -> f a) -> Vector n a -> f (Vector n a)
ix = V.ix
{-# inline ix #-}

-- | Lens to access (/O(1)/) and update (/O(n)/) the first element of a non-empty vector.
_head :: forall n a f. (Prim a, Functor f)
      => (a -> f a) -> Vector (1+n) a -> f (Vector (1+n) a)
_head = V._head
{-# inline _head #-}

-- | Lens to access (/O(1)/) and update (/O(n)/) the last element of a non-empty vector.
_last :: forall n a f. (Prim a, Functor f)
       => (a -> f a) -> Vector (n+1) a -> f (Vector (n+1) a)
_last = V._last
{-# inline _last #-}


-- | /O(1)/ Safe indexing in a monad. See the documentation for 'Data.Vector.Generic.Sized.indexM' for
-- an explanation of why this is useful.
indexM :: forall n a m. (Prim a, Monad m)
      => Vector n a -> Finite n -> m a
indexM = V.indexM
{-# inline indexM #-}

-- | /O(1)/ Safe indexing in a monad using a 'Proxy'. See the documentation for
-- 'Data.Vector.Generic.Sized.indexM' for an explanation of why this is useful.
indexM' :: forall n k a m p. (KnownNat n, Prim a, Monad m)
      => Vector (n+k) a -> p n -> m a
indexM' = V.indexM'
{-# inline indexM' #-}

-- | /O(1)/ Indexing using an 'Int' without bounds checking. See the
-- documentation for 'Data.Vector.Generic.Sized.indexM' for an explanation of why this is useful.
unsafeIndexM :: forall n a m. (Prim a, Monad m)
      => Vector n a -> Int -> m a
unsafeIndexM = V.unsafeIndexM
{-# inline unsafeIndexM #-}

-- | /O(1)/ Yield the first element of a non-empty vector in a monad. See the
-- documentation for 'Data.Vector.Generic.Sized.indexM' for an explanation of why this is useful.
headM :: forall n a m. (Prim a, Monad m)
      => Vector (1+n) a -> m a
headM = V.headM
{-# inline headM #-}

-- | /O(1)/ Yield the last element of a non-empty vector in a monad. See the
-- documentation for 'Data.Vector.Generic.Sized.indexM' for an explanation of why this is useful.
lastM :: forall n a m. (Prim a, Monad m)
      => Vector (n+1) a -> m a
lastM = V.lastM
{-# inline lastM #-}

-- | /O(1)/ Yield a slice of the vector without copying it with an inferred
-- length argument.
slice :: forall i n m a p. (KnownNat i, KnownNat n, Prim a)
      => p i -- ^ starting index
      -> Vector (i+n+m) a
      -> Vector n a
slice = V.slice
{-# inline slice #-}

-- | /O(1)/ Yield a slice of the vector without copying it with an explicit
-- length argument.
slice' :: forall i n m a p. (KnownNat i, KnownNat n, Prim a)
       => p i -- ^ starting index
       -> p n -- ^ length
       -> Vector (i+n+m) a
       -> Vector n a
slice' = V.slice'
{-# inline slice' #-}

-- | /O(1)/ Yield all but the last element of a non-empty vector without
-- copying.
init :: forall n a. (Prim a)
     => Vector (n+1) a -> Vector n a
init = V.init
{-# inline init #-}

-- | /O(1)/ Yield all but the first element of a non-empty vector without
-- copying.
tail :: forall n a. (Prim a)
     => Vector (1+n) a -> Vector n a
tail = V.tail
{-# inline tail #-}

-- | /O(1)/ Yield the first @n@ elements. The resulting vector always contains
-- this many elements. The length of the resulting vector is inferred from the
-- type.
take :: forall n m a. (KnownNat n, Prim a)
     => Vector (n+m) a -> Vector n a
take = V.take
{-# inline take #-}

-- | /O(1)/ Yield the first @n@ elements. The resulting vector always contains
-- this many elements. The length of the resulting vector is given explicitly
-- as a 'Proxy' argument.
take' :: forall n m a p. (KnownNat n, Prim a)
      => p n -> Vector (n+m) a -> Vector n a
take' = V.take'
{-# inline take' #-}

-- | /O(1)/ Yield all but the the first @n@ elements. The given vector must
-- contain at least this many elements. The length of the resulting vector is
-- inferred from the type.
drop :: forall n m a. (KnownNat n, Prim a)
     => Vector (n+m) a -> Vector m a
drop = V.drop
{-# inline drop #-}

-- | /O(1)/ Yield all but the the first @n@ elements. The given vector must
-- contain at least this many elements. The length of the resulting vector is
-- givel explicitly as a 'Proxy' argument.
drop' :: forall n m a p. (KnownNat n, Prim a)
      => p n -> Vector (n+m) a -> Vector m a
drop' = V.drop'
{-# inline drop' #-}

-- | /O(1)/ Yield the first @n@ elements, paired with the rest, without copying.
-- The lengths of the resulting vectors are inferred from the type.
splitAt :: forall n m a. (KnownNat n, Prim a)
        => Vector (n+m) a -> (Vector n a, Vector m a)
splitAt = V.splitAt
{-# inline splitAt #-}

-- | /O(1)/ Yield the first @n@ elements paired with the remainder without
-- copying. The length of the first resulting vector is passed explicitly as a
-- 'Proxy' argument.
splitAt' :: forall n m a p. (KnownNat n, Prim a)
         => p n -> Vector (n+m) a -> (Vector n a, Vector m a)
splitAt' = V.splitAt'
{-# inline splitAt' #-}

--------------------------------------------------------------------------------
-- * Construction
--------------------------------------------------------------------------------

--
-- ** Initialization
--

-- | /O(1)/ Empty vector.
empty :: forall a. (Prim a)
      => Vector 0 a
empty = V.empty
{-# inline empty #-}

-- | /O(1)/ Vector with exactly one element.
singleton :: forall a. (Prim a)
           => a -> Vector 1 a
singleton = V.singleton
{-# inline singleton #-}

-- | /O(n)/ Construct a vector in a type safe manner
-- @
--   fromTuple (1,2) :: Vector 2 Int
--   fromTuple ("hey", "what's", "going", "on") :: Vector 4 String
-- @
fromTuple :: forall a input length.
             (Prim a, IndexedListLiterals input length a, KnownNat length)
          => input -> Vector length a
fromTuple = V.fromTuple
{-# inline fromTuple #-}

-- | /O(n)/ Construct a vector with the same element in each position where the
-- length is inferred from the type.
replicate :: forall n a. (KnownNat n, Prim a)
          => a -> Vector n a
replicate = V.replicate
{-# inline replicate #-}

-- | /O(n)/ Construct a vector with the same element in each position where the
-- length is given explicitly as a 'Proxy' argument.
replicate' :: forall n a p. (KnownNat n, Prim a)
           => p n -> a -> Vector n a
replicate' = V.replicate'
{-# inline replicate' #-}

-- | /O(n)/ construct a vector of the given length by applying the function to
-- each index where the length is inferred from the type.
generate :: forall n a. (KnownNat n, Prim a)
         => (Finite n -> a) -> Vector n a
generate = V.generate
{-# inline generate #-}

-- | /O(n)/ construct a vector of the given length by applying the function to
-- each index where the length is given explicitly as a 'Proxy' argument.
generate' :: forall n a p. (KnownNat n, Prim a)
          => p n -> (Finite n -> a) -> Vector n a
generate' = V.generate'
{-# inline generate' #-}

-- | /O(n)/ Apply function @n@ times to value. Zeroth element is original value.
-- The length is inferred from the type.
iterateN :: forall n a. (KnownNat n, Prim a)
         => (a -> a) -> a -> Vector n a
iterateN = V.iterateN
{-# inline iterateN #-}

-- | /O(n)/ Apply function @n@ times to value. Zeroth element is original value.
-- The length is given explicitly as a 'Proxy' argument.
iterateN' :: forall n a p. (KnownNat n, Prim a)
          => p n -> (a -> a) -> a -> Vector n a
iterateN' = V.iterateN'
{-# inline iterateN' #-}

--
-- ** Monadic initialisation
--

-- | /O(n)/ Execute the monadic action @n@ times and store the results in a
-- vector where @n@ is inferred from the type.
replicateM :: forall n m a. (KnownNat n, Prim a, Monad m)
           => m a -> m (Vector n a)
replicateM = V.replicateM
{-# inline replicateM #-}

-- | /O(n)/ Execute the monadic action @n@ times and store the results in a
-- vector where @n@ is given explicitly as a 'Proxy' argument.
replicateM' :: forall n m a p. (KnownNat n, Prim a, Monad m)
            => p n -> m a -> m (Vector n a)
replicateM' = V.replicateM'
{-# inline replicateM' #-}

-- | /O(n)/ Construct a vector of length @n@ by applying the monadic action to
-- each index where @n@ is inferred from the type.
generateM :: forall n m a. (KnownNat n, Prim a, Monad m)
          => (Finite n -> m a) -> m (Vector n a)
generateM = V.generateM
{-# inline generateM #-}

-- | /O(n)/ Construct a vector of length @n@ by applying the monadic action to
-- each index where @n@ is given explicitly as a 'Proxy' argument.
generateM' :: forall n m a p. (KnownNat n, Prim a, Monad m)
           => p n -> (Finite n -> m a) -> m (Vector n a)
generateM' = V.generateM'
{-# inline generateM' #-}

--
-- ** Unfolding
--

-- | /O(n)/ Construct a vector with exactly @n@ elements by repeatedly applying
-- the generator function to the a seed. The length is inferred from the
-- type.
unfoldrN :: forall n a b. (KnownNat n, Prim a)
         => (b -> (a, b)) -> b -> Vector n a
unfoldrN = V.unfoldrN
{-# inline unfoldrN #-}

-- | /O(n)/ Construct a vector with exactly @n@ elements by repeatedly applying
-- the generator function to the a seed. The length is given explicitly
-- as a 'Proxy' argument.
unfoldrN' :: forall n a b p. (KnownNat n, Prim a)
          => p n -> (b -> (a, b)) -> b -> Vector n a
unfoldrN' = V.unfoldrN'
{-# inline unfoldrN' #-}

--
-- ** Enumeration
--

-- | /O(n)/ Yield a vector of length @n@ containing the values @x@, @x+1@, ...,
-- @x + (n - 1)@. The length is inferred from the type.
enumFromN :: forall n a. (KnownNat n, Prim a, Num a)
          => a -> Vector n a
enumFromN = V.enumFromN
{-# inline enumFromN #-}

-- | /O(n)/ Yield a vector of length @n@ containing the values @x@, @x+1@, ...,
-- @x + (n - 1)@. The length is given explicitly as a 'Proxy' argument.
enumFromN' :: forall n a p. (KnownNat n, Prim a, Num a)
           => a -> p n -> Vector n a
enumFromN' = V.enumFromN'
{-# inline enumFromN' #-}

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+2y@, ..., @x + (n - 1)y@. The length is inferred from the type.
enumFromStepN :: forall n a. (KnownNat n, Prim a, Num a)
          => a -> a -> Vector n a
enumFromStepN = V.enumFromStepN
{-# inline enumFromStepN #-}

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+2y@, ..., @x + (n - 1)y@. The length is given explicitly as a 'Proxy' argument.
enumFromStepN' :: forall n a p. (KnownNat n, Prim a, Num a)
               => a -> a -> p n -> Vector n a
enumFromStepN' = V.enumFromStepN'
{-# inline enumFromStepN' #-}

--
-- ** Concatenation
--

-- | /O(n)/ Prepend an element.
cons :: forall n a. Prim a
     => a -> Vector n a -> Vector (1+n) a
cons = V.cons
{-# inline cons #-}

-- | /O(n)/ Append an element.
snoc :: forall n a. Prim a
     => Vector n a -> a -> Vector (n+1) a
snoc = V.snoc
{-# inline snoc #-}

-- | /O(m+n)/ Concatenate two vectors.
(++) :: forall n m a. Prim a
     => Vector n a -> Vector m a -> Vector (n+m) a
(++) = (V.++)
{-# inline (++) #-}

--
-- ** Restricting memory usage
--

-- | /O(n)/ Yield the argument but force it not to retain any extra memory,
-- possibly by copying it.
--
-- This is especially useful when dealing with slices. For example:
--
-- > force (slice 0 2 <huge vector>)
--
-- Here, the slice retains a reference to the huge vector. Forcing it creates
-- a copy of just the elements that belong to the slice and allows the huge
-- vector to be garbage collected.
force :: Prim a => Vector n a -> Vector n a
force = V.force
{-# inline force #-}


--------------------------------------------------------------------------------
-- * Modifying vectors
--------------------------------------------------------------------------------

--
-- ** Bulk updates
--

-- | /O(m+n)/ For each pair @(i,a)@ from the list, replace the vector
-- element at position @i@ by @a@.
--
-- > <5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
--
(//) :: (Prim a)
     => Vector m a      -- ^ initial vector (of length @m@)
     -> [(Finite m, a)] -- ^ list of index/value pairs (of length @n@)
     -> Vector m a
(//) = (V.//)
{-# inline (//) #-}

-- | /O(m+n)/ For each index @i@ from the index vector and the
-- corresponding value @a@ from the value vector, replace the element of the
-- initial vector at position @i@ by @a@.
--
-- > update_ <5,9,2,7>  <2,0,2> <1,3,8> = <3,9,8,7>
--
-- This function is useful for instances of 'Vector' that cannot store pairs.
-- Otherwise, 'update' is probably more convenient.
--
-- @
-- update_ xs is ys = 'update' xs ('zip' is ys)
-- @
update_ :: Prim a
        => Vector m a   -- ^ initial vector (of length @m@)
        -> Vector n Int -- ^ index vector (of length @n@)
        -> Vector n a   -- ^ value vector (of length @n@)
        -> Vector m a
update_ = V.update_
{-# inline update_ #-}

-- | Same as ('//') but without bounds checking.
unsafeUpd :: (Prim a)
          => Vector m a -- ^ initial vector (of length @m@)
          -> [(Int, a)]   -- ^ list of index/value pairs (of length @n@)
          -> Vector m a
unsafeUpd = V.unsafeUpd
{-# inline unsafeUpd #-}

-- | Same as 'update_' but without bounds checking.
unsafeUpdate_ :: Prim a
              => Vector m a   -- ^ initial vector (of length @m@)
              -> Vector n Int -- ^ index vector (of length @n@)
              -> Vector n a   -- ^ value vector (of length @n@)
              -> Vector m a
unsafeUpdate_ = V.unsafeUpdate_
{-# inline unsafeUpdate_ #-}

--
-- ** Accumulations
--

-- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the vector element
-- @a@ at position @i@ by @f a b@.
--
-- > accum (+) <5,9,2> [(2,4),(1,6),(0,3),(1,7)] = <5+3, 9+6+7, 2+4>
accum :: Prim a
      => (a -> b -> a) -- ^ accumulating function @f@
      -> Vector m a  -- ^ initial vector (of length @m@)
      -> [(Int,b)]     -- ^ list of index/value pairs (of length @n@)
      -> Vector m a
accum = V.accum
{-# inline accum #-}

-- | /O(m+n)/ For each index @i@ from the index vector and the
-- corresponding value @b@ from the the value vector,
-- replace the element of the initial vector at
-- position @i@ by @f a b@.
--
-- > accumulate_ (+) <5,9,2> <2,1,0,1> <4,6,3,7> = <5+3, 9+6+7, 2+4>
--
-- This function is useful for instances of 'Vector' that cannot store pairs.
-- Otherwise, 'accumulate' is probably more convenient:
--
-- @
-- accumulate_ f as is bs = 'accumulate' f as ('zip' is bs)
-- @
accumulate_ :: (Prim a, Prim b)
            => (a -> b -> a)  -- ^ accumulating function @f@
            -> Vector m a   -- ^ initial vector (of length @m@)
            -> Vector n Int -- ^ index vector (of length @n@)
            -> Vector n b   -- ^ value vector (of length @n@)
            -> Vector m a
accumulate_ = V.accumulate_
{-# inline accumulate_ #-}

-- | Same as 'accum' but without bounds checking.
unsafeAccum :: Prim a
            => (a -> b -> a) -- ^ accumulating function @f@
            -> Vector m a  -- ^ initial vector (of length @m@)
            -> [(Int,b)]     -- ^ list of index/value pairs (of length @n@)
            -> Vector m a
unsafeAccum = V.unsafeAccum
{-# inline unsafeAccum #-}

-- | Same as 'accumulate_' but without bounds checking.
unsafeAccumulate_ :: (Prim a, Prim b)
            => (a -> b -> a)  -- ^ accumulating function @f@
            -> Vector m a   -- ^ initial vector (of length @m@)
            -> Vector n Int -- ^ index vector (of length @n@)
            -> Vector n b   -- ^ value vector (of length @n@)
            -> Vector m a
unsafeAccumulate_ = V.unsafeAccumulate_
{-# inline unsafeAccumulate_ #-}

--
-- ** Permutations
--

-- | /O(n)/ Reverse a vector.
reverse :: (Prim a) => Vector n a -> Vector n a
reverse = V.reverse
{-# inline reverse #-}

-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index vector by @xs'!'i@. This is equivalent to @'map' (xs'!') is@ but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
backpermute :: Prim a
            => Vector m a   -- ^ @xs@ value vector
            -> Vector n Int -- ^ @is@ index vector (of length @n@)
            -> Vector n a
backpermute = V.backpermute
{-# inline backpermute #-}

-- | Same as 'backpermute' but without bounds checking.
unsafeBackpermute :: Prim a
                  => Vector m a   -- ^ @xs@ value vector
                  -> Vector n Int -- ^ @is@ index vector (of length @n@)
                  -> Vector n a
unsafeBackpermute = V.unsafeBackpermute
{-# inline unsafeBackpermute #-}

--------------------------------------------------------------------------------
-- * Elementwise Operations
--------------------------------------------------------------------------------

--
-- ** Mapping
--

-- | /O(n)/ Map a function over a vector.
map :: (Prim a, Prim b)
    => (a -> b) -> Vector n a -> Vector n b
map = V.map
{-# inline map #-}

-- | /O(n)/ Apply a function to every element of a vector and its index.
imap :: (Prim a, Prim b)
     => (Finite n -> a -> b) -> Vector n a -> Vector n b
imap = V.imap
{-# inline imap #-}

-- | /O(n*m)/ Map a function over a vector and concatenate the results. The
-- function is required to always return the same length vector.
concatMap :: (Prim a, Prim b)
          => (a -> Vector m b) -> Vector n a -> Vector (n*m) b
concatMap = V.concatMap
{-# inline concatMap #-}

--
-- ** Monadic mapping
--

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results.
mapM :: (Monad m, Prim a, Prim b)
      => (a -> m b) -> Vector n a -> m (Vector n b)
mapM = V.mapM
{-# inline mapM #-}

-- | /O(n)/ Apply the monadic action to every element of a vector and its
-- index, yielding a vector of results.
imapM :: (Monad m, Prim a, Prim b)
      => (Finite n -> a -> m b) -> Vector n a -> m (Vector n b)
imapM = V.imapM
{-# inline imapM #-}

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results.
mapM_ :: (Monad m, Prim a) => (a -> m b) -> Vector n a -> m ()
mapM_ = V.mapM_
{-# inline mapM_ #-}

-- | /O(n)/ Apply the monadic action to every element of a vector and its
-- index, ignoring the results.
imapM_ :: (Monad m, Prim a) => (Finite n -> a -> m b) -> Vector n a -> m ()
imapM_ = V.imapM_
{-# inline imapM_ #-}

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results. Equvalent to @flip 'mapM'@.
forM :: (Monad m, Prim a, Prim b)
     => Vector n a -> (a -> m b) -> m (Vector n b)
forM = V.forM
{-# inline forM #-}

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results. Equivalent to @flip 'mapM_'@.
forM_ :: (Monad m, Prim a) => Vector n a -> (a -> m b) -> m ()
forM_ = V.forM_
{-# inline forM_ #-}

--
-- ** Zipping
--

-- | /O(n)/ Zip two vectors of the same length with the given function.
zipWith :: (Prim a, Prim b, Prim c)
        => (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWith = V.zipWith
{-# inline zipWith #-}

-- | Zip three vectors with the given function.
zipWith3 :: (Prim a, Prim b, Prim c, Prim d)
         => (a -> b -> c -> d) -> Vector n a -> Vector n b -> Vector n c -> Vector n d
zipWith3 = V.zipWith3
{-# inline zipWith3 #-}

zipWith4 :: (Prim a,Prim b,Prim c,Prim d,Prim e)
         => (a -> b -> c -> d -> e)
         -> Vector n a
         -> Vector n b
         -> Vector n c
         -> Vector n d
         -> Vector n e
zipWith4 = V.zipWith4
{-# inline zipWith4 #-}

zipWith5 :: (Prim a,Prim b,Prim c,Prim d,Prim e,Prim f)
         => (a -> b -> c -> d -> e -> f)
         -> Vector n a
         -> Vector n b
         -> Vector n c
         -> Vector n d
         -> Vector n e
         -> Vector n f
zipWith5 = V.zipWith5
{-# inline zipWith5 #-}

zipWith6 :: (Prim a,Prim b,Prim c,Prim d,Prim e,Prim f,Prim g)
         => (a -> b -> c -> d -> e -> f -> g)
         -> Vector n a
         -> Vector n b
         -> Vector n c
         -> Vector n d
         -> Vector n e
         -> Vector n f
         -> Vector n g
zipWith6 = V.zipWith6
{-# inline zipWith6 #-}

-- | /O(n)/ Zip two vectors of the same length with a function that also takes
-- the elements' indices).
izipWith :: (Prim a,Prim b,Prim c)
         => (Finite n -> a -> b -> c)
         -> Vector n a
         -> Vector n b
         -> Vector n c
izipWith = V.izipWith
{-# inline izipWith #-}

izipWith3 :: (Prim a,Prim b,Prim c,Prim d)
          => (Finite n -> a -> b -> c -> d)
          -> Vector n a
          -> Vector n b
          -> Vector n c
          -> Vector n d
izipWith3 = V.izipWith3
{-# inline izipWith3 #-}

izipWith4 :: (Prim a,Prim b,Prim c,Prim d,Prim e)
          => (Finite n -> a -> b -> c -> d -> e)
          -> Vector n a
          -> Vector n b
          -> Vector n c
          -> Vector n d
          -> Vector n e
izipWith4 = V.izipWith4
{-# inline izipWith4 #-}

izipWith5 :: (Prim a,Prim b,Prim c,Prim d,Prim e,Prim f)
          => (Finite n -> a -> b -> c -> d -> e -> f)
          -> Vector n a
          -> Vector n b
          -> Vector n c
          -> Vector n d
          -> Vector n e
          -> Vector n f
izipWith5 = V.izipWith5
{-# inline izipWith5 #-}

izipWith6 :: (Prim a,Prim b,Prim c,Prim d,Prim e,Prim f,Prim g)
          => (Finite n -> a -> b -> c -> d -> e -> f -> g)
          -> Vector n a
          -> Vector n b
          -> Vector n c
          -> Vector n d
          -> Vector n e
          -> Vector n f
          -> Vector n g
izipWith6 = V.izipWith6
{-# inline izipWith6 #-}

--
-- ** Monadic zipping
--

-- | /O(n)/ Zip the two vectors of the same length with the monadic action and
-- yield a vector of results.
zipWithM :: (Monad m, Prim a, Prim b, Prim c)
         => (a -> b -> m c) -> Vector n a -> Vector n b -> m (Vector n c)
zipWithM = V.zipWithM
{-# inline zipWithM #-}

-- | /O(n)/ Zip the two vectors with a monadic action that also takes the
-- element index and yield a vector of results.
izipWithM :: (Monad m, Prim a, Prim b, Prim c)
         => (Finite n -> a -> b -> m c) -> Vector n a -> Vector n b -> m (Vector n c)
izipWithM = V.izipWithM
{-# inline izipWithM #-}

-- | /O(n)/ Zip the two vectors with the monadic action and ignore the results.
zipWithM_ :: (Monad m, Prim a, Prim b)
          => (a -> b -> m c) -> Vector n a -> Vector n b -> m ()
zipWithM_ = V.zipWithM_
{-# inline zipWithM_ #-}

-- | /O(n)/ Zip the two vectors with a monadic action that also takes
-- the element index and ignore the results.
izipWithM_ :: (Monad m, Prim a, Prim b)
           => (Finite n -> a -> b -> m c) -> Vector n a -> Vector n b -> m ()
izipWithM_ = V.izipWithM_
{-# inline izipWithM_ #-}

--------------------------------------------------------------------------------
-- * Working with predicates
--------------------------------------------------------------------------------

--
-- ** Searching
--


infix 4 `elem`
-- | /O(n)/ Check if the vector contains an element.
elem :: (Prim a, Eq a) => a -> Vector n a -> Bool
elem = V.elem
{-# inline elem #-}

infix 4 `notElem`
-- | /O(n)/ Check if the vector does not contain an element (inverse of 'elem').
notElem :: (Prim a, Eq a) => a -> Vector n a -> Bool
notElem = V.notElem
{-# inline notElem #-}

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: Prim a => (a -> Bool) -> Vector n a -> Maybe a
find = V.find
{-# inline find #-}

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: Prim a => (a -> Bool) -> Vector n a -> Maybe (Finite n)
findIndex = V.findIndex
{-# inline findIndex #-}

-- | /O(n)/ Yield 'Just' the index of the first occurence of the given element or
-- 'Nothing' if the vector does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: (Prim a, Eq a) => a -> Vector n a -> Maybe (Finite n)
elemIndex = V.elemIndex
{-# inline elemIndex #-}

--------------------------------------------------------------------------------
-- * Folding
--------------------------------------------------------------------------------

-- | /O(n)/ Left fold.
foldl :: Prim b => (a -> b -> a) -> a -> Vector n b -> a
foldl = V.foldl
{-# inline foldl #-}

-- | /O(n)/ Left fold on non-empty vectors.
foldl1 :: Prim a => (a -> a -> a) -> Vector (1+n) a -> a
foldl1 = V.foldl1
{-# inline foldl1 #-}

-- | /O(n)/ Left fold with strict accumulator.
foldl' :: Prim b => (a -> b -> a) -> a -> Vector n b -> a
foldl' = V.foldl'
{-# inline foldl' #-}

-- | /O(n)/ Left fold on non-empty vectors with strict accumulator.
foldl1' :: Prim a => (a -> a -> a) -> Vector (1+n) a -> a
foldl1' = V.foldl1'
{-# inline foldl1' #-}

-- | /O(n)/ Right fold.
foldr :: Prim a => (a -> b -> b) -> b -> Vector n a -> b
foldr = V.foldr
{-# inline foldr #-}

-- | /O(n)/ Right fold on non-empty vectors.
foldr1 :: Prim a => (a -> a -> a) -> Vector (n+1) a -> a
foldr1 = V.foldr1
{-# inline foldr1 #-}

-- | /O(n)/ Right fold with a strict accumulator.
foldr' :: Prim a => (a -> b -> b) -> b -> Vector n a -> b
foldr' = V.foldr'
{-# inline foldr' #-}

-- | /O(n)/ Right fold on non-empty vectors with strict accumulator.
foldr1' :: Prim a => (a -> a -> a) -> Vector (n+1) a -> a
foldr1' = V.foldr1'
{-# inline foldr1' #-}

-- | /O(n)/ Left fold (function applied to each element and its index).
ifoldl :: Prim b => (a -> Finite n -> b -> a) -> a -> Vector n b -> a
ifoldl = V.ifoldl
{-# inline ifoldl #-}

-- | /O(n)/ Left fold with strict accumulator (function applied to each element
-- and its index).
ifoldl' :: Prim b => (a -> Finite n -> b -> a) -> a -> Vector n b -> a
ifoldl' = V.ifoldl'
{-# inline ifoldl' #-}

-- | /O(n)/ Right fold (function applied to each element and its index).
ifoldr :: Prim a => (Finite n -> a -> b -> b) -> b -> Vector n a -> b
ifoldr = V.ifoldr
{-# inline ifoldr #-}

-- | /O(n)/ Right fold with strict accumulator (function applied to each
-- element and its index).
ifoldr' :: Prim a => (Finite n -> a -> b -> b) -> b -> Vector n a -> b
ifoldr' = V.ifoldr'
{-# inline ifoldr' #-}

-- ** Specialised folds

-- | /O(n)/ Check if all elements satisfy the predicate.
all :: Prim a => (a -> Bool) -> Vector n a -> Bool
all = V.all
{-# inline all #-}

-- | /O(n)/ Check if any element satisfies the predicate.
any :: Prim a => (a -> Bool) -> Vector n a -> Bool
any = V.any
{-# inline any #-}

-- | /O(n)/ Compute the sum of the elements.
sum :: (Prim a, Num a) => Vector n a -> a
sum = V.sum
{-# inline sum #-}

-- | /O(n)/ Compute the product of the elements.
product :: (Prim a, Num a) => Vector n a -> a
product = V.product
{-# inline product #-}

-- | /O(n)/ Yield the maximum element of the non-empty vector.
maximum :: (Prim a, Ord a) => Vector (n+1) a -> a
maximum = V.maximum
{-# inline maximum #-}

-- | /O(n)/ Yield the maximum element of the non-empty vector according to the
-- given comparison function.
maximumBy :: Prim a
          => (a -> a -> Ordering) -> Vector (n+1) a -> a
maximumBy = V.maximumBy
{-# inline maximumBy #-}

-- | /O(n)/ Yield the minimum element of the non-empty vector.
minimum :: (Prim a, Ord a) => Vector (n+1) a -> a
minimum = V.minimum
{-# inline minimum #-}

-- | /O(n)/ Yield the minimum element of the non-empty vector according to the
-- given comparison function.
minimumBy :: Prim a
          => (a -> a -> Ordering) -> Vector (n+1) a -> a
minimumBy = V.minimumBy
{-# inline minimumBy #-}

-- | /O(n)/ Yield the index of the maximum element of the non-empty vector.
maxIndex :: (Prim a, Ord a) => Vector (n+1) a -> Finite (n + 1)
maxIndex = V.maxIndex
{-# inline maxIndex #-}

-- | /O(n)/ Yield the index of the maximum element of the non-empty vector
-- according to the given comparison function.
maxIndexBy :: Prim a
           => (a -> a -> Ordering) -> Vector (n+1) a -> Finite (n + 1)
maxIndexBy = V.maxIndexBy
{-# inline maxIndexBy #-}

-- | /O(n)/ Yield the index of the minimum element of the non-empty vector.
minIndex :: (Prim a, Ord a) => Vector (n+1) a -> Finite (n + 1)
minIndex = V.minIndex
{-# inline minIndex #-}

-- | /O(n)/ Yield the index of the minimum element of the non-empty vector
-- according to the given comparison function.
minIndexBy :: Prim a
           => (a -> a -> Ordering) -> Vector (n+1) a -> Finite (n + 1)
minIndexBy = V.minIndexBy
{-# inline minIndexBy #-}

-- ** Monadic folds

-- | /O(n)/ Monadic fold.
foldM :: (Monad m, Prim b) => (a -> b -> m a) -> a -> Vector n b -> m a
foldM = V.foldM
{-# inline foldM #-}

-- | /O(n)/ Monadic fold (action applied to each element and its index).
ifoldM :: (Monad m, Prim b) => (a -> Finite n -> b -> m a) -> a -> Vector n b -> m a
ifoldM = V.ifoldM
{-# inline ifoldM #-}

-- | /O(n)/ Monadic fold over non-empty vectors.
fold1M :: (Monad m, Prim a)
       => (a -> a -> m a) -> Vector (1+n) a -> m a
fold1M = V.fold1M
{-# inline fold1M #-}

-- | /O(n)/ Monadic fold with strict accumulator.
foldM' :: (Monad m, Prim b) => (a -> b -> m a) -> a -> Vector n b -> m a
foldM' = V.foldM'
{-# inline foldM' #-}

-- | /O(n)/ Monadic fold with strict accumulator (action applied to each
-- element and its index).
ifoldM' :: (Monad m, Prim b)
        => (a -> Finite n -> b -> m a) -> a -> Vector n b -> m a
ifoldM' = V.ifoldM'
{-# inline ifoldM' #-}

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator.
fold1M' :: (Monad m, Prim a)
        => (a -> a -> m a) -> Vector (n+1) a -> m a
fold1M' = V.fold1M'
{-# inline fold1M' #-}

-- | /O(n)/ Monadic fold that discards the result.
foldM_ :: (Monad m, Prim b)
       => (a -> b -> m a) -> a -> Vector n b -> m ()
foldM_ = V.foldM_
{-# inline foldM_ #-}

-- | /O(n)/ Monadic fold that discards the result (action applied to
-- each element and its index).
ifoldM_ :: (Monad m, Prim b)
        => (a -> Finite n -> b -> m a) -> a -> Vector n b -> m ()
ifoldM_ = V.ifoldM_
{-# inline ifoldM_ #-}

-- | /O(n)/ Monadic fold over non-empty vectors that discards the result.
fold1M_ :: (Monad m, Prim a)
        => (a -> a -> m a) -> Vector (n+1) a -> m ()
fold1M_ = V.fold1M_
{-# inline fold1M_ #-}

-- | /O(n)/ Monadic fold with strict accumulator that discards the result.
foldM'_ :: (Monad m, Prim b)
        => (a -> b -> m a) -> a -> Vector n b -> m ()
foldM'_ = V.foldM'_
{-# inline foldM'_ #-}

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
-- (action applied to each element and its index).
ifoldM'_ :: (Monad m, Prim b)
         => (a -> Finite n -> b -> m a) -> a -> Vector n b -> m ()
ifoldM'_ = V.ifoldM'_
{-# inline ifoldM'_ #-}

-- | /O(n)/ Monad fold over non-empty vectors with strict accumulator
-- that discards the result.
fold1M'_ :: (Monad m, Prim a)
         => (a -> a -> m a) -> Vector (n+1) a -> m ()
fold1M'_ = V.fold1M'_
{-# inline fold1M'_ #-}

--------------------------------------------------------------------------------
-- * Prefix sums (scans)
--------------------------------------------------------------------------------

-- | /O(n)/ Prescan.
--
-- @
-- prescanl f z = 'init' . 'scanl' f z
-- @
--
-- Example: @prescanl (+) 0 \<1,2,3,4\> = \<0,1,3,6\>@
--
prescanl :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector n b -> Vector n a
prescanl = V.prescanl
{-# inline prescanl #-}

-- | /O(n)/ Prescan with strict accumulator.
prescanl' :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector n b -> Vector n a
prescanl' = V.prescanl'
{-# inline prescanl' #-}

-- | /O(n)/ Scan.
postscanl :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector n b -> Vector n a
postscanl = V.postscanl
{-# inline postscanl #-}

-- | /O(n)/ Scan with strict accumulator.
postscanl' :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector n b -> Vector n a
postscanl' = V.postscanl'
{-# inline postscanl' #-}

-- | /O(n)/ Haskell-style scan.
scanl :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector n b -> Vector (1+n) a
scanl = V.scanl
{-# inline scanl #-}

-- | /O(n)/ Haskell-style scan with strict accumulator.
scanl' :: (Prim a, Prim b) => (a -> b -> a) -> a -> Vector n b -> Vector (1+n) a
scanl' = V.scanl'
{-# inline scanl' #-}

-- | /O(n)/ Scan over a non-empty vector.
scanl1 :: Prim a => (a -> a -> a) -> Vector (1+n) a -> Vector (2+n) a
scanl1 = V.scanl1
{-# inline scanl1 #-}

-- | /O(n)/ Scan over a non-empty vector with a strict accumulator.
scanl1' :: Prim a => (a -> a -> a) -> Vector (1+n) a -> Vector (2+n) a
scanl1' = V.scanl1'
{-# inline scanl1' #-}

-- | /O(n)/ Right-to-left prescan.
prescanr :: (Prim a, Prim b) => (a -> b -> b) -> b -> Vector n a -> Vector n b
prescanr = V.prescanr
{-# inline prescanr #-}

-- | /O(n)/ Right-to-left prescan with strict accumulator.
prescanr' :: (Prim a, Prim b) => (a -> b -> b) -> b -> Vector n a -> Vector n b
prescanr' = V.prescanr'
{-# inline prescanr' #-}

-- | /O(n)/ Right-to-left scan.
postscanr :: (Prim a, Prim b) => (a -> b -> b) -> b -> Vector n a -> Vector n b
postscanr = V.postscanr
{-# inline postscanr #-}

-- | /O(n)/ Right-to-left scan with strict accumulator.
postscanr' :: (Prim a, Prim b) => (a -> b -> b) -> b -> Vector n a -> Vector n b
postscanr' = V.postscanr'
{-# inline postscanr' #-}

-- | /O(n)/ Right-to-left Haskell-style scan.
scanr :: (Prim a, Prim b) => (a -> b -> b) -> b -> Vector n a -> Vector (n+1) b
scanr = V.scanr
{-# inline scanr #-}

-- | /O(n)/ Right-to-left Haskell-style scan with strict accumulator.
scanr' :: (Prim a, Prim b) => (a -> b -> b) -> b -> Vector n a -> Vector (n+1) b
scanr' = V.scanr'
{-# inline scanr' #-}

-- | /O(n)/ Right-to-left scan over a non-empty vector.
scanr1 :: Prim a => (a -> a -> a) -> Vector (n+1) a -> Vector (n+2) a
scanr1 = V.scanr1
{-# inline scanr1 #-}

-- | /O(n)/ Right-to-left scan over a non-empty vector with a strict
-- accumulator.
scanr1' :: Prim a => (a -> a -> a) -> Vector (n+1) a -> Vector (n+2) a
scanr1' = V.scanr1'
{-# inline scanr1' #-}


-- * Conversions

-- ** Lists

-- | /O(n)/ Convert a vector to a list.
toList :: Prim a => Vector n a -> [a]
toList = V.toList
{-# inline toList #-}

-- | /O(n)/ Convert a list to a vector.
fromList :: (Prim a, KnownNat n) => [a] -> Maybe (Vector n a)
fromList = V.fromList
{-# inline fromList #-}

-- | /O(n)/ Convert the first @n@ elements of a list to a vector. The length of
-- the resulting vector is inferred from the type.
fromListN :: forall n a. (Prim a, KnownNat n)
          => [a] -> Maybe (Vector n a)
fromListN = V.fromListN
{-# inline fromListN #-}

-- | /O(n)/ Convert the first @n@ elements of a list to a vector. The length of
-- the resulting vector is given explicitly as a 'Proxy' argument.
fromListN' :: forall n a p. (Prim a, KnownNat n)
           => p n -> [a] -> Maybe (Vector n a)
fromListN' = V.fromListN'
{-# inline fromListN' #-}

-- | /O(n)/ Takes a list and returns a continuation providing a vector with
-- a size parameter corresponding to the length of the list.
--
-- Essentially converts a list into a vector with the proper size
-- parameter, determined at runtime.
--
-- See 'withSized'
withSizedList :: forall a r. Prim a
              => [a] -> (forall n. KnownNat n => Vector n a -> r) -> r
withSizedList xs = withSized (VS.fromList xs)
{-# inline withSizedList #-}

-- ** Mutable vectors

-- | /O(n)/ Yield an immutable copy of the mutable vector.
freeze :: (PrimMonad m, Prim a)
       => VSM.MVector n (PrimState m) a
       -> m (Vector n a)
freeze = V.freeze

-- | /O(1)/ Unsafely convert a mutable vector to an immutable one withouy
-- copying. The mutable vector may not be used after this operation.
unsafeFreeze :: (PrimMonad m, Prim a)
             => VSM.MVector n (PrimState m) a
             -> m (Vector n a)
unsafeFreeze = V.unsafeFreeze

-- | /O(n)/ Yield a mutable copy of the immutable vector.
thaw :: (PrimMonad m, Prim a)
     => Vector n a
     -> m (VSM.MVector n (PrimState m) a)
thaw = V.thaw

-- | /O(n)/ Unsafely convert an immutable vector to a mutable one without
-- copying. The immutable vector may not be used after this operation.
unsafeThaw :: (PrimMonad m, Prim a)
           => Vector n a
           -> m (VSM.MVector n (PrimState m) a)
unsafeThaw = V.unsafeThaw

-- | /O(n)/ Copy an immutable vector into a mutable one.
copy :: (PrimMonad m, Prim a)
     => VSM.MVector n (PrimState m) a
     -> Vector n a
     -> m ()
copy = V.copy

-- ** Unsized vectors

-- | Convert a 'Data.Vector.Generic.Vector' into a
-- 'Data.Vector.Generic.Sized.Vector' if it has the correct size, otherwise
-- return 'Nothing'.
toSized :: forall n a. (Prim a, KnownNat n)
        => VS.Vector a -> Maybe (Vector n a)
toSized = V.toSized
{-# inline toSized #-}

-- | Takes a 'Data.Vector.Primitive.Vector' and returns a continuation
-- providing a 'Data.Vector.Primitive.Sized.Vector' with a size parameter
-- @n@ that is determined at runtime based on the length of the input
-- vector.
--
-- Essentially converts a 'Data.Vector.Primitive.Vector' into
-- a 'Data.Vector.Primitive.Sized.Vector' with the correct size parameter
-- @n@.
withSized :: forall a r. Prim a
          => VS.Vector a -> (forall n. KnownNat n => Vector n a -> r) -> r
withSized = V.withSized
{-# inline withSized #-}

fromSized :: Vector n a -> VS.Vector a
fromSized = V.fromSized
{-# inline fromSized #-}

-- | Apply a function on unsized vectors to a sized vector. The function must
-- preserve the size of the vector, this is not checked.
withVectorUnsafe :: forall a b (n :: Nat). ()
                 => (VS.Vector a -> VS.Vector b) -> Vector n a -> Vector n b
withVectorUnsafe = V.withVectorUnsafe
{-# inline withVectorUnsafe #-}

-- | Pattern synonym that lets you treat an unsized vector as if it
-- "contained" a sized vector.  If you pattern match on an unsized vector,
-- its contents will be the /sized/ vector counterpart.
--
-- @
-- testFunc :: Unsized.Vector Int -> Int
-- testFunc ('SomeSized' v) =
--     'sum' ('zipWith' (+) v ('replicate' 1))
--         -- ^ here, v is `Sized.Vector n Int`, and we have
--                     `'KnownNat' n`
-- @
--
-- The @n@ type variable will be properly instantiated to whatever the
-- length of the vector is, and you will also have a @'KnownNat' n@
-- instance available.  You can get @n@ in scope by turning on
-- ScopedTypeVariables and matching on @'SomeSized' (v :: Sized.Vector
-- n Int)@.
--
-- Without this, you would otherwise have to use 'withSized' to do the same
-- thing:
--
-- @
-- testFunc :: Unsized.Vector Int -> Int
-- testFunc u = 'withSized' u $ \\v ->
--     'sum' ('zipWith' (+) v ('replicate' 1))
-- @
--
-- Remember that the type of final result of your function (the @Int@,
-- here) must /not/ depend on @n@.  However, the types of the intermediate
-- values are allowed to depend on @n@.
--
-- This is /especially/ useful in do blocks, where you can pattern match on
-- the unsized results of actions, to use the sized vector in the rest of
-- the do block.  You also get a @'KnownNat' n@ constraint for the
-- remainder of the do block.
--
-- @
-- -- If you had:
-- getAVector :: IO (Unsized.Vector Int)
--
-- main :: IO ()
-- main = do
--     SomeSized v <- getAVector -- v is `Sized.Vector n Int`
--     -- get n in scope
--     SomeSized (v :: Sized.Vector n Int) <- getAVector
--     print v
-- @
--
-- Remember that the final type of the result of the do block ('()', here)
-- must not depend on @n@.  However, the
--
-- Also useful in ghci, where you can pattern match to get sized vectors
-- from unsized vectors.
--
-- @
-- ghci> SomeSized v <- pure (myUnsizedVector :: Unsized.Vector Int)
--              -- ^ v is `Sized.Vector n Int`
-- @
--
-- This enables interactive exploration with sized vectors in ghci, and is
-- useful for using with other libraries and functions that expect sized
-- vectors in an interactive setting.
--
-- (Note that as of GHC 8.6, you cannot get the @n@ in scope in your ghci
-- session using ScopedTypeVariables, like you can with do blocks)
--
-- You can also use this as a constructor, to take a sized vector and
-- "hide" the size, to produce an unsized vector:
--
-- @
-- SomeSized :: Sized.Vector n a -> Unsized.Vector a
-- @
pattern SomeSized :: Prim a => KnownNat n => Vector n a -> VS.Vector a
pattern SomeSized v = V.SomeSized v
{-# complete SomeSized #-}
