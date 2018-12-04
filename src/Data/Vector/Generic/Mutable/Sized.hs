{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

{-|
This module reexports the functionality in 'Data.Vector.Generic.Mutable'
which maps well to explicitly sized vectors.

Functions returning a vector determine the size from the type context
unless they have a @'@ suffix in which case they take an explicit 'Proxy'
argument.

Functions where the resultant vector size is not known until runtime
are not exported.
-}

module Data.Vector.Generic.Mutable.Sized
 ( MVector
   -- * Accessors
   -- ** Length information
  , length
  , length'
  , null
   -- ** Extracting subvectors
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
  -- ** Overlaps
  , overlaps
  -- * Construction
  -- ** Initialisation
  , new
  , unsafeNew
  , replicate
  , replicate'
  , replicateM
  , replicateM'
  , clone
  -- ** Growing
  , grow
  , growFront
  -- ** Restricting memory usage
  , clear
  -- * Accessing individual elements
  , read
  , read'
  , write
  , write'
  , modify
  , modify'
  , swap
  , exchange
  , exchange'
  , unsafeRead
  , unsafeWrite
  , unsafeModify
  , unsafeSwap
  , unsafeExchange
#if MIN_VERSION_vector(0,12,0)
  -- * Modifying vectors
  , nextPermutation
#endif
  -- ** Filling and copying
  , set
  , copy
  , move
  , unsafeCopy
    -- * Conversions
    -- ** Unsized Mutable Vectors
  , toSized
  , withSized
  , fromSized
  ) where

import qualified Data.Vector.Generic.Mutable as VGM
import Data.Vector.Generic.Mutable.Sized.Internal
import GHC.TypeLits
import Data.Finite
import Data.Finite.Internal
import Data.Maybe
import Data.Proxy
import Control.Monad.Primitive
import Prelude hiding ( length, null, replicate, init,
                        tail, take, drop, splitAt, read )

-- * Accessors

-- ** Length information

-- | /O(1)/ Yield the length of the mutable vector as an 'Int'.
length :: forall v n s a. KnownNat n
       => MVector v n s a -> Int
length _ = fromInteger (natVal (Proxy :: Proxy n))
{-# inline length #-}

-- | /O(1)/ Yield the length of the mutable vector as a 'Proxy'.
length' :: forall v n s a. ()
        => MVector v n s a -> Proxy n
length' _ = Proxy
{-# inline length' #-}

-- | /O(1)/ Check whether the mutable vector is empty.
null :: forall v n s a. KnownNat n
       => MVector v n s a -> Bool
null _ = isJust $ Proxy @n `sameNat` Proxy @0
{-# inline null #-}

-- ** Extracting subvectors

-- | /O(1)/ Yield a slice of the mutable vector without copying it with an
-- inferred length argument.
slice :: forall v i n k s a p. (KnownNat i, KnownNat n, VGM.MVector v a)
      => p i -- ^ starting index
      -> MVector v (i+n+k) s a
      -> MVector v n s a
slice start (MVector v) = MVector (VGM.unsafeSlice i n v)
  where i = fromInteger (natVal start)
        n = fromInteger (natVal (Proxy :: Proxy n))
{-# inline slice #-}

-- | /O(1)/ Yield a slice of the mutable vector without copying it with an
-- explicit length argument.
slice' :: forall v i n k s a p
        . (KnownNat i, KnownNat n, VGM.MVector v a)
       => p i -- ^ starting index
       -> p n -- ^ length
       -> MVector v (i+n+k) s a
       -> MVector v n s a
slice' start _ = slice start
{-# inline slice' #-}

-- | /O(1)/ Yield all but the last element of a non-empty mutable vector
-- without copying.
init :: forall v n s a. VGM.MVector v a
     => MVector v (n+1) s a -> MVector v n s a
init (MVector v) = MVector (VGM.unsafeInit v)
{-# inline init #-}

-- | /O(1)/ Yield all but the first element of a non-empty mutable vector
-- without copying.
tail :: forall v n s a. VGM.MVector v a
     => MVector v (1+n) s a -> MVector v n s a
tail (MVector v) = MVector (VGM.unsafeTail v)
{-# inline tail #-}

-- | /O(1)/ Yield the first @n@ elements. The resulting vector always contains
-- this many elements. The length of the resulting vector is inferred from the
-- type.
take :: forall v n k s a. (KnownNat n, VGM.MVector v a)
     => MVector v (n+k) s a -> MVector v n s a
take (MVector v) = MVector (VGM.unsafeTake i v)
  where i = fromInteger (natVal (Proxy :: Proxy n))
{-# inline take #-}

-- | /O(1)/ Yield the first @n@ elements. The resulting vector always contains
-- this many elements. The length of the resulting vector is given explicitly
-- as a 'Proxy' argument.
take' :: forall v n k s a p. (KnownNat n, VGM.MVector v a)
      => p n -> MVector v (n+k) s a -> MVector v n s a
take' _ = take
{-# inline take' #-}

-- | /O(1)/ Yield all but the the first @n@ elements. The given vector must
-- contain at least this many elements. The length of the resulting vector is
-- inferred from the type.
drop :: forall v n k s a. (KnownNat n, VGM.MVector v a)
     => MVector v (n+k) s a -> MVector v k s a
drop (MVector v) = MVector (VGM.unsafeDrop i v)
  where i = fromInteger (natVal (Proxy :: Proxy n))
{-# inline drop #-}

-- | /O(1)/ Yield all but the the first @n@ elements. The given vector must
-- contain at least this many elements. The length of the resulting vector is
-- given explicitly as a 'Proxy' argument.
drop' :: forall v n k s a p. (KnownNat n, VGM.MVector v a)
      => p n -> MVector v (n+k) s a -> MVector v k s a
drop' _ = drop
{-# inline drop' #-}

-- | /O(1)/ Yield the first @n@ elements, paired with the rest, without copying.
-- The lengths of the resulting vectors are inferred from the type.
splitAt :: forall v n m s a. (KnownNat n, VGM.MVector v a)
        => MVector v (n+m) s a -> (MVector v n s a, MVector v m s a)
splitAt (MVector v) = (MVector a, MVector b)
  where i = fromInteger (natVal (Proxy :: Proxy n))
        (a, b) = VGM.splitAt i v
{-# inline splitAt #-}

-- | /O(1)/ Yield the first @n@ elements, paired with the rest, without
-- copying. The length of the first resulting vector is passed explicitly as a
-- 'Proxy' argument.
splitAt' :: forall v n m s a p. (KnownNat n, VGM.MVector v a)
         => p n -> MVector v (n+m) s a -> (MVector v n s a, MVector v m s a)
splitAt' _ = splitAt
{-# inline splitAt' #-}

-- ** Overlaps

-- | /O(1)/ Check whether two vectors overlap. 
overlaps :: forall v n k s a. VGM.MVector v a
         => MVector v n s a
         -> MVector v k s a
         -> Bool
overlaps (MVector v) (MVector u) = VGM.overlaps v u
{-# inline overlaps #-}

-- * Construction

-- ** Initialisation

-- | Create a mutable vector where the length is inferred from the type.
new :: forall v n m a. (KnownNat n, PrimMonad m, VGM.MVector v a)
    => m (MVector v n (PrimState m) a)
new = MVector <$> VGM.new (fromIntegral (natVal (Proxy :: Proxy n)))
{-# inline new #-}

-- | Create a mutable vector where the length is inferred from the type.
-- The memory is not initialized.
unsafeNew :: forall v n m a. (KnownNat n, PrimMonad m, VGM.MVector v a)
          => m (MVector v n (PrimState m) a)
unsafeNew = MVector <$> VGM.new (fromIntegral (natVal (Proxy :: Proxy n)))
{-# inline unsafeNew #-}

-- | Create a mutable vector where the length is inferred from the type and
-- fill it with an initial value.
replicate :: forall v n m a. (KnownNat n, PrimMonad m, VGM.MVector v a)
          => a -> m (MVector v n (PrimState m) a)
replicate = fmap MVector . VGM.replicate (fromIntegral (natVal (Proxy :: Proxy n)))
{-# inline replicate #-}

-- | Create a mutable vector where the length is given explicitly as
-- a 'Proxy' argument and fill it with an initial value.
replicate' :: forall v n m a p. (KnownNat n, PrimMonad m, VGM.MVector v a)
           => p n -> a -> m (MVector v n (PrimState m) a)
replicate' _ = replicate
{-# inline replicate' #-}

-- | Create a mutable vector where the length is inferred from the type and
-- fill it with values produced by repeatedly executing the monadic action.
replicateM :: forall v n m a. (KnownNat n, PrimMonad m, VGM.MVector v a)
           => m a -> m (MVector v n (PrimState m) a)
replicateM = fmap MVector . VGM.replicateM (fromIntegral (natVal (Proxy :: Proxy n)))
{-# inline replicateM #-}

-- | Create a mutable vector where the length is given explicitly as
-- a 'Proxy' argument and fill it with values produced by repeatedly
-- executing the monadic action.
replicateM' :: forall v n m a p. (KnownNat n, PrimMonad m, VGM.MVector v a)
           => p n -> m a -> m (MVector v n (PrimState m) a)
replicateM' _ = replicateM
{-# inline replicateM' #-}

-- | Create a copy of a mutable vector.
clone :: forall v n m a. (PrimMonad m, VGM.MVector v a)
      => MVector v n (PrimState m) a -> m (MVector v n (PrimState m) a)
clone (MVector v) = MVector <$> VGM.clone v
{-# inline clone #-}

-- ** Growing

-- | Grow a mutable vector by an amount given explicitly as a 'Proxy'
-- argument.
grow :: forall v n k m a p. (KnownNat k, PrimMonad m, VGM.MVector v a)
      => p k -> MVector v n (PrimState m) a -> m (MVector v (n + k) (PrimState m) a)
grow _ (MVector v) = MVector <$> VGM.unsafeGrow v (fromIntegral (natVal (Proxy :: Proxy k)))
{-# inline grow #-}

-- | Grow a mutable vector (from the front) by an amount given explicitly
-- as a 'Proxy' argument.
growFront :: forall v n k m a p. (KnownNat k, PrimMonad m, VGM.MVector v a)
      => p k -> MVector v n (PrimState m) a -> m (MVector v (n + k) (PrimState m) a)
growFront _ (MVector v) = MVector <$>
    VGM.unsafeGrowFront v (fromIntegral (natVal (Proxy :: Proxy k)))
{-# inline growFront #-}

-- ** Restricting memory usage

-- | Reset all elements of the vector to some undefined value, clearing all
-- references to external objects.
clear :: (PrimMonad m, VGM.MVector v a) => MVector v n (PrimState m) a -> m ()
clear (MVector v) = VGM.clear v
{-# inline clear #-}

-- * Accessing individual elements

-- | /O(1)/ Yield the element at a given type-safe position using 'Finite'.
read :: forall v n m a. (PrimMonad m, VGM.MVector v a)
      => MVector v n (PrimState m) a -> Finite n -> m a
read (MVector v) (Finite i) = v `VGM.unsafeRead` fromIntegral i
{-# inline read #-}

-- | /O(1)/ Yield the element at a given type-safe position using 'Proxy'.
read' :: forall v n k a m p. (KnownNat k, PrimMonad m, VGM.MVector v a)
       => MVector v (n+k+1) (PrimState m) a -> p k -> m a
read' (MVector v) p = v `VGM.unsafeRead` fromInteger (natVal p)
{-# inline read' #-}

-- | /O(1)/ Yield the element at a given 'Int' position without bounds
-- checking.
unsafeRead :: forall v n a m. (PrimMonad m, VGM.MVector v a)
           => MVector v n (PrimState m) a -> Int -> m a
unsafeRead (MVector v) i = v `VGM.unsafeRead` i
{-# inline unsafeRead #-}

-- | /O(1)/ Replace the element at a given type-safe position using 'Finite'.
write :: forall v n m a. (PrimMonad m, VGM.MVector v a)
      => MVector v n (PrimState m) a -> Finite n -> a -> m ()
write (MVector v) (Finite i) = VGM.unsafeWrite v (fromIntegral i)
{-# inline write #-}

-- | /O(1)/ Replace the element at a given type-safe position using 'Proxy'.
write' :: forall v n k a m p. (KnownNat k, PrimMonad m, VGM.MVector v a)
       => MVector v (n+k+1) (PrimState m) a -> p k -> a -> m ()
write' (MVector v) p = VGM.unsafeWrite v (fromInteger (natVal p))
{-# inline write' #-}

-- | /O(1)/ Replace the element at a given 'Int' position without bounds
-- checking.
unsafeWrite :: forall v n m a. (PrimMonad m, VGM.MVector v a)
      => MVector v n (PrimState m) a -> Int -> a -> m ()
unsafeWrite (MVector v) = VGM.unsafeWrite v
{-# inline unsafeWrite #-}

-- | /O(1)/ Modify the element at a given type-safe position using 'Finite'.
modify :: forall v n m a. (PrimMonad m, VGM.MVector v a)
       => MVector v n (PrimState m) a -> (a -> a) -> Finite n -> m ()
modify (MVector v) f (Finite i) = VGM.unsafeModify v f (fromIntegral i)
{-# inline modify #-}

-- | /O(1)/ Modify the element at a given type-safe position using 'Proxy'.
modify' :: forall v n k a m p. (KnownNat k, PrimMonad m, VGM.MVector v a)
        => MVector v (n+k+1) (PrimState m) a -> (a -> a) -> p k -> m ()
modify' (MVector v) f p = VGM.unsafeModify v f (fromInteger (natVal p))
{-# inline modify' #-}

-- | /O(1)/ Modify the element at a given 'Int' position without bounds
-- checking.
unsafeModify :: forall v n m a. (PrimMonad m, VGM.MVector v a)
       => MVector v n (PrimState m) a -> (a -> a) -> Int -> m ()
unsafeModify (MVector v) = VGM.unsafeModify v
{-# inline unsafeModify #-}

-- | /O(1)/ Swap the elements at given type-safe positions using 'Finite's.
swap :: forall v n m a. (PrimMonad m, VGM.MVector v a)
     => MVector v n (PrimState m) a -> Finite n -> Finite n -> m ()
swap (MVector v) (Finite i) (Finite j) = VGM.unsafeSwap v (fromIntegral i) (fromIntegral j)
{-# inline swap #-}

-- | /O(1)/ Swap the elements at given 'Int' positions without bounds
-- checking.
unsafeSwap :: forall v n m a. (PrimMonad m, VGM.MVector v a)
           => MVector v n (PrimState m) a -> Int -> Int -> m ()
unsafeSwap (MVector v) = VGM.unsafeSwap v
{-# inline unsafeSwap #-}

-- | /O(1)/ Replace the element at a given type-safe position and return
-- the old element, using 'Finite'.
exchange :: forall v n m a. (PrimMonad m, VGM.MVector v a)
         => MVector v n (PrimState m) a -> Finite n -> a -> m a
exchange (MVector v) (Finite i) = VGM.unsafeExchange v (fromIntegral i)
{-# inline exchange #-}

-- | /O(1)/ Replace the element at a given type-safe position and return
-- the old element, using 'Finite'.
exchange' :: forall v n k a m p. (KnownNat k, PrimMonad m, VGM.MVector v a)
          => MVector v (n+k+1) (PrimState m) a -> p k -> a -> m a
exchange' (MVector v) p = VGM.unsafeExchange v (fromInteger (natVal p))
{-# inline exchange' #-}

-- | /O(1)/ Replace the element at a given 'Int' position and return
-- the old element. No bounds checks are performed.
unsafeExchange :: forall v n m a. (PrimMonad m, VGM.MVector v a)
         => MVector v n (PrimState m) a -> Int -> a -> m a
unsafeExchange (MVector v) = VGM.unsafeExchange v
{-# inline unsafeExchange #-}

#if MIN_VERSION_vector(0,12,0)
-- * Modifying vectors

-- | Compute the next permutation (in lexicographic order) of a given vector
-- in-place.  Returns 'False' when the input is the last permutation.
nextPermutation :: forall v n e m. (Ord e, PrimMonad m, VGM.MVector v e)
                => MVector v n (PrimState m) e -> m Bool
nextPermutation (MVector v) = VGM.nextPermutation v
{-# inline nextPermutation #-}
#endif

-- ** Filling and copying

-- | Set all elements of the vector to the given value.
set :: (PrimMonad m, VGM.MVector v a) => MVector v n (PrimState m) a -> a -> m ()
set (MVector v) = VGM.set v
{-# inline set #-}

-- | Copy a vector. The two vectors may not overlap.
copy :: (PrimMonad m, VGM.MVector v a)
     => MVector v n (PrimState m) a       -- ^ target
     -> MVector v n (PrimState m) a       -- ^ source
     -> m ()
copy (MVector v) (MVector u)
    | v `VGM.overlaps` u = error "copy: overlapping vectors"
    | otherwise          = VGM.unsafeCopy v u
{-# inline copy #-}

-- | Copy a vector. The two vectors may not overlap. This is not checked.
unsafeCopy :: (PrimMonad m, VGM.MVector v a)
           => MVector v n (PrimState m) a       -- ^ target
           -> MVector v n (PrimState m) a       -- ^ source
           -> m ()
unsafeCopy (MVector v) (MVector u) = VGM.unsafeCopy v u
{-# inline unsafeCopy #-}

-- | Move the contents of a vector.  If the two vectors do not overlap,
-- this is equivalent to 'copy'.  Otherwise, the copying is performed as if
-- the source vector were copied to a temporary vector and then the
-- temporary vector was copied to the target vector.
move :: (PrimMonad m, VGM.MVector v a)
     => MVector v n (PrimState m) a       -- ^ target
     -> MVector v n (PrimState m) a       -- ^ source
     -> m ()
move (MVector v) (MVector u) = VGM.unsafeMove v u
{-# inline move #-}

-- * Conversions

-- ** Unsized Mutable Vectors

-- | Convert a 'Data.Vector.Generic.Mutable.MVector' into
-- a 'Data.Vector.Generic.Mutable.Sized.MVector' if it has the correct
-- size, otherwise return Nothing.
--
-- Note that this does no copying; the returned 'MVector' is a reference to
-- the exact same vector in memory as the given one, and any modifications
-- to it are also reflected in the given
-- 'Data.Vector.Generic.Mutable.MVector'.
toSized :: forall v n s a. (VGM.MVector v a, KnownNat n)
        => v s a -> Maybe (MVector v n s a)
toSized v
  | n' == fromIntegral (VGM.length v) = Just (MVector v)
  | otherwise                         = Nothing
  where n' = natVal (Proxy :: Proxy n)
{-# inline toSized #-}

-- | Takes a 'Data.Vector.Generic.Mutable.MVector' and returns
-- a continuation providing a 'Data.Vector.Generic.Mutable.Sized.MVector'
-- with a size parameter @n@ that is determined at runtime based on the
-- length of the input vector.
--
-- Essentially converts a 'Data.Vector.Generic.Mutable.MVector' into
-- a 'Data.Vector.Generic.Sized.MVector' with the correct size parameter
-- @n@.
--
-- Note that this does no copying; the returned 'MVector' is a reference to
-- the exact same vector in memory as the given one, and any modifications
-- to it are also reflected in the given
-- 'Data.Vector.Generic.Mutable.MVector'.
withSized :: forall v s a r. VGM.MVector v a
          => v s a -> (forall n. KnownNat n => MVector v n s a -> r) -> r
withSized v f = case someNatVal (fromIntegral (VGM.length v)) of
    Just (SomeNat (Proxy :: Proxy n)) -> f (MVector v :: MVector v n s a)
    Nothing -> error "withSized: VGM.length returned negative length."

-- | Convert a 'Data.Vector.Generic.Mutable.Sized.MVector' into a
-- 'Data.Vector.Generic.Mutable.MVector'.
--
-- Note that this does no copying; the returned
-- 'Data.Vector.Generic.Mutable.MVector' is a reference to the exact same
-- vector in memory as the given one, and any modifications to it are also
-- reflected in the given 'MVector'.
fromSized :: MVector v n s a -> v s a
fromSized (MVector v) = v
{-# inline fromSized #-}

