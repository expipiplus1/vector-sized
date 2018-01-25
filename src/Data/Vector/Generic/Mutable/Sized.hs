{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}


module Data.Vector.Generic.Mutable.Sized (
  ) where

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector as Boxed
import GHC.Generics (Generic)
import GHC.TypeLits
import Data.Finite
import Data.Finite.Internal
import Data.Proxy
import Control.DeepSeq (NFData)
import Control.Monad.Primitive
import Foreign.Storable
import Data.Data
import Data.Functor.Classes
import Foreign.Ptr (castPtr)
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

-- | A wrapper to tag mutable vectors with a type level length.
newtype MVector v (n :: Nat) s a = MVector (v s a)
  deriving ( Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic
           , Show1, Eq1, Ord1
           , Data, Typeable
           )

-- | /O(1)/ Yield the length of the mutable vector as an 'Int'.
length :: forall v n s a. (KnownNat n)
       => MVector v n s a -> Int
length _ = fromInteger (natVal (Proxy :: Proxy n))
{-# inline length #-}

-- | /O(1)/ Yield the length of the mutable vector as a 'Proxy'.
length' :: forall v n s a. (KnownNat n)
        => MVector v n s a -> Proxy n
length' _ = Proxy
{-# inline length' #-}

-- | /O(1)/ Check whether the mutable vector is empty
null :: forall v n s a. (KnownNat n)
       => MVector v n s a -> Bool
null = (== 0) . length
{-# inline null #-}

-- | /O(1)/ Yield a slice of the mutable vector without copying it with an
-- inferred length argument.
slice :: forall v i n m s a p. (KnownNat i, KnownNat n, KnownNat m, VGM.MVector v a)
      => p i -- ^ starting index
      -> MVector v (i+n+m) s a
      -> MVector v n s a
slice start (MVector v) = MVector (VGM.unsafeSlice i n v)
  where i = fromInteger (natVal start)
        n = fromInteger (natVal (Proxy :: Proxy n))
{-# inline slice #-}

-- | /O(1)/ Yield a slice of the mutable vector without copying it with an
-- explicit length argument.
slice' :: forall v i n m s a p
        . (KnownNat i, KnownNat n, KnownNat m, VGM.MVector v a)
       => p i -- ^ starting index
       -> p n -- ^ length
       -> MVector v (i+n+m) s a
       -> MVector v n s a
slice' start _ = slice start
{-# inline slice' #-}

-- | /O(1)/ Yield all but the last element of a non-empty mutable vector
-- without copying.
init :: forall v n s a. (VGM.MVector v a)
     => MVector v (n+1) s a -> MVector v n s a
init (MVector v) = MVector (VGM.unsafeInit v)
{-# inline init #-}

-- | /O(1)/ Yield all but the first element of a non-empty mutable vector
-- without copying.
tail :: forall v n s a. (VGM.MVector v a)
     => MVector v (1+n) s a -> MVector v n s a
tail (MVector v) = MVector (VGM.unsafeTail v)
{-# inline tail #-}

-- | /O(1)/ Yield the first n elements. The resultant vector always contains
-- this many elements. The length of the resultant vector is inferred from the
-- type.
take :: forall v n m s a. (KnownNat n, KnownNat m, VGM.MVector v a)
     => MVector v (n+m) s a -> MVector v n s a
take (MVector v) = MVector (VGM.unsafeTake i v)
  where i = fromInteger (natVal (Proxy :: Proxy n))
{-# inline take #-}

-- | /O(1)/ Yield the first n elements. The resultant vector always contains
-- this many elements. The length of the resultant vector is given explicitly
-- as a 'Proxy' argument.
take' :: forall v n m s a p. (KnownNat n, KnownNat m, VGM.MVector v a)
      => p n -> MVector v (n+m) s a -> MVector v n s a
take' _ = take
{-# inline take' #-}

-- | /O(1)/ Yield all but the the first n elements. The given vector must
-- contain at least this many elements The length of the resultant vector is
-- inferred from the type.
drop :: forall v n m s a. (KnownNat n, KnownNat m, VGM.MVector v a)
     => MVector v (n+m) s a -> MVector v m s a
drop (MVector v) = MVector (VGM.unsafeDrop i v)
  where i = fromInteger (natVal (Proxy :: Proxy n))
{-# inline drop #-}

-- | /O(1)/ Yield all but the the first n elements. The given vector must
-- contain at least this many elements The length of the resultant vector is
-- givel explicitly as a 'Proxy' argument.
drop' :: forall v n m s a p. (KnownNat n, KnownNat m, VGM.MVector v a)
      => p n -> MVector v (n+m) s a -> MVector v m s a
drop' _ = drop
{-# inline drop' #-}

-- | /O(1)/ Yield the first n elements paired with the remainder without copying.
-- The lengths of the resultant vector are inferred from the type.
splitAt :: forall v n m s a. (KnownNat n, KnownNat m, VGM.MVector v a)
        => MVector v (n+m) s a -> (MVector v n s a, MVector v m s a)
splitAt (MVector v) = (MVector a, MVector b)
  where i = fromInteger (natVal (Proxy :: Proxy n))
        (a, b) = VGM.splitAt i v
{-# inline splitAt #-}

-- | /O(1)/ Yield the first n elements paired with the remainder without
-- copying.  The length of the first resultant vector is passed explicitly as a
-- 'Proxy' argument.
splitAt' :: forall v n m s a p. (KnownNat n, KnownNat m, VGM.MVector v a)
         => p n -> MVector v (n+m) s a -> (MVector v n s a, MVector v m s a)
splitAt' _ = splitAt
{-# inline splitAt' #-}

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
clone :: forall v n m a. (KnownNat n, PrimMonad m, VGM.MVector v a)
      => MVector v n (PrimState m) a -> m (MVector v n (PrimState m) a)
clone (MVector v) = MVector <$> VGM.clone v
{-# inline clone #-}
