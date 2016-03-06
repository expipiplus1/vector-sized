{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vector.Generic.Sized
 ( Vector
   -- * Accessors
   -- ** Length information
  , length
  , length'
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



    -- * Construction
  , fromVector
    -- * Monadic Construction
    -- * Mapping
  , map
    -- * Monadic Mapping
  , imapM_
    -- * Folding
  , foldl'
  , foldl1'
  ) where

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import GHC.TypeLits
import Data.Proxy
import Control.DeepSeq (NFData)
import Foreign.Storable
import Foreign.Ptr (castPtr)
import Prelude hiding (replicate, head, last, tail, init, map, length, drop,
                       take, splitAt, (++))

newtype Vector v (n :: Nat) a = Vector (v a)
  deriving (Show, Eq, Ord, Foldable, NFData)

instance (KnownNat n, Storable a)
      => Storable (Vector VS.Vector n a) where
  sizeOf _ = sizeOf (undefined :: a) * fromIntegral (natVal (Proxy :: Proxy n))
  alignment _ = alignment (undefined :: a)
  peek ptr = generateM (peekElemOff (castPtr ptr))
  poke ptr = imapM_ (pokeElemOff (castPtr ptr))

-- | /O(1)/ Yield the length of the vector as an 'Int'.
length :: forall v n a. (KnownNat n)
       => Vector v n a -> Int
length _ = fromInteger (natVal (Proxy :: Proxy n))
{-# inline length #-}

-- | /O(1)/ Yield the length of the vector as a 'Proxy'.
length' :: forall v n a. (KnownNat n)
        => Vector v n a -> Proxy n
length' _ = Proxy
{-# inline length' #-}

-- | /O(1)/ Indexing using an Int.
index :: forall v n a. (KnownNat n, VG.Vector v a)
      => Vector v n a -> Int -> a
index (Vector v) i = v VG.! i
{-# inline index #-}

-- | /O(1)/ Safe indexing using a 'Proxy'.
index' :: forall v n m a. (KnownNat n, KnownNat m, VG.Vector v a)
       => Vector v (n+m) a -> Proxy n -> a
index' (Vector v) p = v `VG.unsafeIndex` i
  where i = fromInteger (natVal p)
{-# inline index' #-}

-- | /O(1)/ Indexing using an Int without bounds checking.
unsafeIndex :: forall v n a. (KnownNat n, VG.Vector v a)
      => Vector v n a -> Int -> a
unsafeIndex (Vector v) i = v `VG.unsafeIndex` i
{-# inline unsafeIndex #-}

-- | /O(1)/ Yield the first element of a non-empty vector.
head :: forall v n a. (VG.Vector v a)
     => Vector v (n+1) a -> a
head (Vector v) = VG.unsafeHead v
{-# inline head #-}

-- | /O(1)/ Yield the last element of a non-empty vector.
last :: forall v n a. (VG.Vector v a)
     => Vector v (n+1) a -> a
last (Vector v) = VG.unsafeLast v
{-# inline last #-}

-- | /O(1)/ Indexing in a monad. See the documentation for 'VG.indexM' for an
-- explanation of why this is useful.
indexM :: forall v n a m. (KnownNat n, VG.Vector v a, Monad m)
      => Vector v n a -> Int -> m a
indexM (Vector v) i = v `VG.indexM` i
{-# inline indexM #-}

-- | /O(1)/ Safe indexing in a monad using a 'Proxy'. See the documentation for
-- 'VG.indexM' for an explanation of why this is useful.
indexM' :: forall v n k a m. (KnownNat n, KnownNat k, VG.Vector v a, Monad m)
      => Vector v (n+k) a -> Proxy n -> m a
indexM' (Vector v) p = v `VG.indexM` i
  where i = fromInteger (natVal p)
{-# inline indexM' #-}

-- | /O(1)/ Indexing using an Int without bounds checking. See the
-- documentation for 'VG.indexM' for an explanation of why this is useful.
unsafeIndexM :: forall v n a m. (KnownNat n, VG.Vector v a, Monad m)
      => Vector v n a -> Int -> m a
unsafeIndexM (Vector v) i = v `VG.unsafeIndexM` i
{-# inline unsafeIndexM #-}

-- | /O(1)/ Yield the first element of a non-empty vector in a monad. See the
-- documentation for 'VG.indexM' for an explanation of why this is useful.
headM :: forall v n a m. (KnownNat n, VG.Vector v a, Monad m)
      => Vector v (n+1) a -> m a
headM (Vector v) = VG.unsafeHeadM v
{-# inline headM #-}

-- | /O(1)/ Yield the last element of a non-empty vector in a monad. See the
-- documentation for 'VG.indexM' for an explanation of why this is useful.
lastM :: forall v n a m. (KnownNat n, VG.Vector v a, Monad m)
      => Vector v (n+1) a -> m a
lastM (Vector v) = VG.unsafeLastM v
{-# inline lastM #-}

-- | /O(1)/ Yield a slice of the vector without copying it with an inferred
-- length argument.
slice :: forall v i n a. (KnownNat i, KnownNat n, VG.Vector v a)
      => Proxy i -- ^ starting index
      -> Vector v (i+n) a
      -> Vector v n a
slice pi (Vector v) = Vector (VG.unsafeSlice i n v)
  where i = fromInteger (natVal pi)
        n = fromInteger (natVal (Proxy :: Proxy n))
{-# inline slice #-}

-- | /O(1)/ Yield a slice of the vector without copying it with an explicit
-- length argument.
slice' :: forall v i n a. (KnownNat i, KnownNat n, VG.Vector v a)
       => Proxy i -- ^ starting index
       -> Proxy n -- ^ length
       -> Vector v (i+n) a
       -> Vector v n a
slice' pi _ = slice pi
{-# inline slice' #-}

-- | /O(1)/ Yield all but the last element of a non-empty vector without
-- copying.
init :: forall v n a. (VG.Vector v a)
     => Vector v (n+1) a -> Vector v n a
init (Vector v) = Vector (VG.unsafeInit v)
{-# inline init #-}

-- | /O(1)/ Yield all but the first element of a non-empty vector without
-- copying.
tail :: forall v n a. (VG.Vector v a)
     => Vector v (n+1) a -> Vector v n a
tail (Vector v) = Vector (VG.unsafeTail v)
{-# inline tail #-}

-- | /O(1)/ Yield the first n elements. The resultant vector always contains
-- this many elements. The length of the resultant vector is inferred from the
-- type.
take :: forall v n m a. (KnownNat n, KnownNat m, VG.Vector v a)
     => Vector v (m+n) a -> Vector v n a
take (Vector v) = Vector (VG.unsafeTake i v)
  where i = fromInteger (natVal (Proxy :: Proxy n))
{-# inline take #-}

-- | /O(1)/ Yield the first n elements. The resultant vector always contains
-- this many elements. The length of the resultant vector is given explicitly
-- as a 'Proxy' argument.
take' :: forall v n m a. (KnownNat n, KnownNat m, VG.Vector v a)
      => Proxy n -> Vector v (m+n) a -> Vector v n a
take' _ = take
{-# inline take' #-}

-- | /O(1)/ Yield all but the the first n elements. The given vector must
-- contain at least this many elements The length of the resultant vector is
-- inferred from the type.
drop :: forall v n m a. (KnownNat n, KnownNat m, VG.Vector v a)
     => Vector v (m+n) a -> Vector v m a
drop (Vector v) = Vector (VG.unsafeDrop i v)
  where i = fromInteger (natVal (Proxy :: Proxy n))
{-# inline drop #-}

-- | /O(1)/ Yield all but the the first n elements. The given vector must
-- contain at least this many elements The length of the resultant vector is
-- givel explicitly as a 'Proxy' argument.
drop' :: forall v n m a. (KnownNat n, KnownNat m, VG.Vector v a)
      => Proxy n -> Vector v (m+n) a -> Vector v m a
drop' _ = drop
{-# inline drop' #-}

-- | /O(1)/ Yield the first n elements paired with the remainder without copying.
-- The lengths of the resultant vector are inferred from the type.
splitAt :: forall v n m a. (KnownNat n, KnownNat m, VG.Vector v a)
        => Vector v (n+m) a -> (Vector v n a, Vector v m a)
splitAt (Vector v) = (Vector a, Vector b)
  where i = fromInteger (natVal (Proxy :: Proxy n))
        (a, b) = VG.splitAt i v
{-# inline splitAt #-}

-- | /O(1)/ Yield the first n elements paired with the remainder without
-- copying.  The length of the first resultant vector is passed explicitly as a
-- 'Proxy' argument.
splitAt' :: forall v n m a. (KnownNat n, KnownNat m, VG.Vector v a)
         => Proxy n -> Vector v (n+m) a -> (Vector v n a, Vector v m a)
splitAt' _ = splitAt
{-# inline splitAt' #-}

--------------------------------------------------------------------------------
-- * Construction
--------------------------------------------------------------------------------

--
-- ** Initialization
--

-- | /O(1)/ Empty vector.
empty :: forall v a. (VG.Vector v a)
      => Vector v 0 a
empty = Vector VG.empty
{-# inline empty #-}

-- | /O(1)/ Vector with exactly one element.
singleton :: forall v a. (VG.Vector v a)
           => a -> Vector v 1 a
singleton a = Vector (VG.singleton a)
{-# inline singleton #-}

-- | /O(n)/ Construct a vector with the same element in each position where the
-- length is inferred from the type.
replicate :: forall v n a. (KnownNat n, VG.Vector v a)
          => a -> Vector v n a
replicate a = Vector (VG.replicate i a)
  where i = fromInteger (natVal (Proxy :: Proxy n))
{-# inline replicate #-}

-- | /O(n)/ Construct a vector with the same element in each position where the
-- length is given explicitly as a 'Proxy' argument.
replicate' :: forall v n a. (KnownNat n, VG.Vector v a)
           => Proxy n -> a -> Vector v n a
replicate' _ = replicate
{-# inline replicate' #-}

-- | /O(n)/ construct a vector of the given length by applying the function to
-- each index where the length is inferred from the type.
generate :: forall v n a. (KnownNat n, VG.Vector v a)
         => (Int -> a) -> Vector v n a
generate f = Vector (VG.generate i f)
  where i = fromInteger (natVal (Proxy :: Proxy n))
{-# inline generate #-}

-- | /O(n)/ construct a vector of the given length by applying the function to
-- each index where the length is given explicitly as a 'Proxy' argument.
generate' :: forall v n a. (KnownNat n, VG.Vector v a)
          => Proxy n -> (Int -> a) -> Vector v n a
generate' _ = generate
{-# inline generate' #-}

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
-- The length is inferred from the type.
iterateN :: forall v n a. (KnownNat n, VG.Vector v a)
         => (a -> a) -> a -> Vector v n a
iterateN f z = Vector (VG.iterateN i f z)
  where i = fromInteger (natVal (Proxy :: Proxy n))
{-# inline iterateN #-}

-- | /O(n)/ Apply function n times to value. Zeroth element is original value.
-- The length is given explicitly as a 'Proxy' argument.
iterateN' :: forall v n a. (KnownNat n, VG.Vector v a)
          => Proxy n -> (a -> a) -> a -> Vector v n a
iterateN' _ = iterateN
{-# inline iterateN' #-}

--
-- ** Monadic initialisation
--

-- | /O(n)/ Execute the monadic action @n@ times and store the results in a
-- vector where @n@ is inferred from the type.
replicateM :: forall v n m a. (KnownNat n, VG.Vector v a, Monad m)
           => m a -> m (Vector v n a)
replicateM a = Vector <$> VG.replicateM i a
  where i = fromInteger (natVal (Proxy :: Proxy n))
{-# inline replicateM #-}

-- | /O(n)/ Execute the monadic action @n@ times and store the results in a
-- vector where @n@ is given explicitly as a 'Proxy' argument.
replicateM' :: forall v n m a. (KnownNat n, VG.Vector v a, Monad m)
            => Proxy n -> m a -> m (Vector v n a)
replicateM' _ = replicateM
{-# inline replicateM' #-}

-- | /O(n)/ Construct a vector of length @n@ by applying the monadic action to
-- each index where n is inferred from the type.
generateM :: forall v n m a. (KnownNat n, VG.Vector v a, Monad m)
          => (Int -> m a) -> m (Vector v n a)
generateM f = Vector <$> VG.generateM i f
  where i = fromInteger (natVal (Proxy :: Proxy n))
{-# inline generateM #-}

-- | /O(n)/ Construct a vector of length @n@ by applying the monadic action to
-- each index where n is given explicitly as a 'Proxy' argument.
generateM' :: forall v n m a. (KnownNat n, VG.Vector v a, Monad m)
           => Proxy n -> (Int -> m a) -> m (Vector v n a)
generateM' _ = generateM
{-# inline generateM' #-}

--
-- ** Unfolding
--

-- | /O(n)/ Construct a vector with exactly @n@ elements by repeatedly applying
-- the generator function to the a seed. The length, @n@, is inferred from the
-- type.
unfoldrN :: forall v n a b. (KnownNat n, VG.Vector v a)
         => (b -> (a, b)) -> b -> Vector v n a
unfoldrN f z = Vector (VG.unfoldrN i (Just . f) z)
  where i = fromInteger (natVal (Proxy :: Proxy n))
{-# inline unfoldrN #-}

-- | /O(n)/ Construct a vector with exactly @n@ elements by repeatedly applying
-- the generator function to the a seed. The length, @n@, is given explicitly
-- as a 'Proxy' argument.
unfoldrN' :: forall v n a b. (KnownNat n, VG.Vector v a)
          => Proxy n -> (b -> (a, b)) -> b -> Vector v n a
unfoldrN' _ = unfoldrN
{-# inline unfoldrN' #-}

--
-- ** Enumeration
-- 

-- | /O(n)/ Yield a vector of length @n@ containing the values @x@, @x+1@
-- etc. The length, @n@, is inferred from the type.
enumFromN :: forall v n a. (KnownNat n, VG.Vector v a, Num a)
          => a -> Vector v n a
enumFromN a = Vector (VG.enumFromN a i)
  where i = fromInteger (natVal (Proxy :: Proxy n))
{-# inline enumFromN #-}

-- | /O(n)/ Yield a vector of length @n@ containing the values @x@, @x+1@
-- etc. The length, @n@, is given explicitly as a 'Proxy' argument.
enumFromN' :: forall v n a. (KnownNat n, VG.Vector v a, Num a)
           => a -> Proxy n -> Vector v n a
enumFromN' a _ = enumFromN a
{-# inline enumFromN' #-}

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc. The length, @n@, is inferred from the type.
enumFromStepN :: forall v n a. (KnownNat n, VG.Vector v a, Num a)
          => a -> a -> Vector v n a
enumFromStepN a a' = Vector (VG.enumFromStepN a a' i)
  where i = fromInteger (natVal (Proxy :: Proxy n))
{-# inline enumFromStepN #-}

-- | /O(n)/ Yield a vector of the given length containing the values @x@, @x+y@,
-- @x+y+y@ etc. The length, @n@, is given explicitly as a 'Proxy' argument.
enumFromStepN' :: forall v n a. (KnownNat n, VG.Vector v a, Num a)
               => a -> a -> Proxy n -> Vector v n a
enumFromStepN' a a' _ = enumFromStepN a a'
{-# inline enumFromStepN' #-}

--
-- ** Concatenation
-- 

-- | /O(n)/ Prepend an element. 
cons :: forall v n a. VG.Vector v a
     => a -> Vector v n a -> Vector v (n+1) a
cons x (Vector xs) = Vector (VG.cons x xs)
{-# inline cons #-}

-- | /O(n)/ Append an element.
snoc :: forall v n a. VG.Vector v a
     => Vector v n a -> a -> Vector v (n+1) a
snoc (Vector xs) x = Vector (VG.snoc xs x)
{-# inline snoc #-}

-- | /O(m+n)/ Concatenate two vectors.
(++) :: forall v n m a. VG.Vector v a
     => Vector v n a -> Vector v m a -> Vector v (n+m) a
Vector vn ++ Vector vm = Vector (vn VG.++ vm)
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
force :: VG.Vector v a => Vector v n a -> Vector v n a
force (Vector v) = Vector (VG.force v)
{-# inline force #-}


-- | Convert a 'Data.Vector.Generic.Vector' into a
-- 'Data.Vector.Generic.Sized.Vector' if it has the correct size, otherwise
-- return Nothing.
fromVector :: forall a v (n :: Nat). (KnownNat n, VG.Vector v a)
           => v a -> Maybe (Vector v n a)
fromVector v
  | n' == fromIntegral (VG.length v) = Just (Vector v)
  | otherwise                        = Nothing
  where n' = natVal (Proxy :: Proxy n)
{-# INLINE fromVector #-}

-- | Apply a function on unsized vectors to a sized vector. The function must
-- preserve the size of the vector, this is not checked.
withVectorUnsafe :: forall a b v (n :: Nat). (VG.Vector v a, VG.Vector v b)
                 => (v a -> v b) -> Vector v n a -> Vector v n b
withVectorUnsafe f (Vector v) = Vector (f v)
{-# INLINE withVectorUnsafe #-}

-- | /O(n)/ Map a function over the vector.
map :: forall a b v (n :: Nat). (VG.Vector v a, VG.Vector v b)
    => (a -> b) -> Vector v n a -> Vector v n b
map f = withVectorUnsafe (VG.map f)
{-# INLINE map #-}

-- | /O(n)/ Apply the monadic action to every element of a vector and its
-- index, ignoring the results.
imapM_ :: forall a v n b m. (VG.Vector v a, Monad m)
       => (Int -> a -> m b) -> Vector v n a -> m ()
imapM_ f (Vector v) = VG.imapM_ f v
{-# INLINE imapM_ #-}

-- | /O(n)/ Left fold with a strict accumulator.
foldl' :: forall a b v (n :: Nat). VG.Vector v b
       => (a -> b -> a) -> a -> Vector v n b -> a
foldl' f z (Vector v) = VG.foldl' f z v
{-# INLINE foldl' #-}

-- | /O(n)/ Left fold on a non-empty vector with a strict accumulator.
foldl1' :: forall a v (n :: Nat). (VG.Vector v a)
        => (a -> a -> a) -> Vector v (n+1) a -> a
foldl1' f (Vector v) = VG.foldl1' f v
{-# INLINE foldl1' #-}
