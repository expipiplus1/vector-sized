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
  , fromVector
  , replicate
  , singleton
  , generate
    -- * Monadic Construction
  , generateM
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
import Control.DeepSeq
import Foreign.Storable
import Foreign.Ptr (castPtr)
import Prelude hiding (replicate, head, last, tail, init, map, length, drop,
                       take, splitAt)

newtype Vector v (n :: Nat) a = Vector (v a)
  deriving (Show, Eq, Ord, Foldable, NFData)

instance (KnownNat n, Storable a) 
      => Storable (Vector VS.Vector n a) where
  sizeOf _ = sizeOf (undefined :: a) * fromIntegral (natVal (Proxy :: Proxy n))
  alignment _ = alignment (undefined :: a)
  peek ptr = generateM (Proxy :: Proxy n) (peekElemOff (castPtr ptr))
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

-- | /O(1)/ construct a single element vector.
singleton :: forall a v. (VG.Vector v a)
          => a -> Vector v 1 a
singleton a = Vector (VG.singleton a)
{-# INLINE singleton #-}

-- | /O(n)/ construct a vector of the given length by applying the function to
-- each index.
generate :: forall (n :: Nat) a v. (VG.Vector v a, KnownNat n)
         => Proxy n -> (Int -> a) -> Vector v n a
generate n f = Vector (VG.generate (fromIntegral $ natVal n) f)
{-# INLINE generate #-}

-- | /O(n)/ construct a vector of the given length by applying the monadic
-- action to each index.
generateM :: forall (n :: Nat) a v m. (VG.Vector v a, KnownNat n, Monad m)
         => Proxy n -> (Int -> m a) -> m (Vector v n a)
generateM n f = Vector <$> VG.generateM (fromIntegral $ natVal n) f
{-# INLINE generateM #-}

-- | Apply a function on unsized vectors to a sized vector. The function must
-- preserve the size of the vector, this is not checked.
withVectorUnsafe :: forall a b v (n :: Nat). (VG.Vector v a, VG.Vector v b)
                 => (v a -> v b) -> Vector v n a -> Vector v n b
withVectorUnsafe f (Vector v) = Vector (f v)
{-# INLINE withVectorUnsafe #-}
-- | /O(n)/ Construct a vector with the same element in each position.
replicate :: forall a v (n :: Nat). (VG.Vector v a, KnownNat n)
          => a -> Vector v n a
replicate a = Vector (VG.replicate (fromIntegral $ natVal (Proxy :: Proxy n)) a)
{-# INLINE replicate #-}

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
