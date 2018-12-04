{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS_HADDOCK not-home            #-}

module Data.Vector.Generic.Sized.Internal
  ( Vector(..)
  )
where

import           Control.DeepSeq                ( NFData )
import           Data.Data                      ( Data
                                                , Typeable
                                                )
import           Data.Functor.Classes           ( Eq1
                                                , Ord1
                                                , Show1
                                                )
import           Data.Vector                   as V
                                                ( and
                                                , foldl'
                                                , null
                                                , zipWith
                                                , zipWith3
                                                )
import qualified Data.Vector.Generic           as VG
                                                ( Vector
                                                , convert
                                                , empty
                                                , fromList
                                                , toList
                                                )
import           GHC.Arr                        ( Ix
                                                  ( inRange
                                                  , range
                                                  , unsafeIndex
                                                  , unsafeRangeSize
                                                  )
                                                )
import           GHC.Generics                   ( Generic )
import           GHC.TypeLits                   ( Nat )

-- | A wrapper to tag vectors with a type level length.
--
-- Be careful when using the constructor here to not construct sized vectors
-- which have a different length than that specified in the type parameter!
newtype Vector v (n :: Nat) a = Vector (v a)
  deriving ( Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic
           , Show1, Eq1, Ord1
           , Data, Typeable
           )

instance (Ix a, Ord (v a), VG.Vector v a) => Ix (Vector v n a) where

  -- range is consistent with range :: ((a,..,a), (a,..,a)) -> [(a,..,a)]
  range (Vector l, Vector u) = Vector <$> enumerate ranges
   where
    ranges = V.zipWith (curry range) lc uc
    lc     = VG.convert l
    uc     = VG.convert u
    enumerate v | V.null v  = [VG.empty]
                | otherwise = map VG.fromList $ enumerate' (VG.toList v)
    enumerate' []         = [[]]
    enumerate' (xs : xss) = [ x : xs' | x <- xs, xs' <- enumerate' xss ]

-- index/unsafeIndex is consistent with
-- index :: ((a,..,a), (a,..,a)) -> (a,..,a) -> Int
  unsafeIndex (Vector l, Vector u) (Vector i) = V.foldl' f 0 v
   where
    f acc (index', rangeSize') = acc * rangeSize' + index'
    v            = V.zipWith3 indexAndRangeSize lc uc ic
    (lc, uc, ic) = convert3 l u i
    indexAndRangeSize l' u' i' =
      let b' = (l', u') in (unsafeIndex b' i', unsafeRangeSize b')

-- i is in range (l, u) if, and only if, that is true for all elements,
-- element-by-element
  inRange (Vector l, Vector u) (Vector i) = V.and
    $ V.zipWith3 (curry inRange) lc uc ic
    where (lc, uc, ic) = convert3 l u i

-- Conversion helper
{-# INLINE convert3 #-}
convert3
  :: ( VG.Vector v1 a
     , VG.Vector w1 a
     , VG.Vector v2 b
     , VG.Vector w2 b
     , VG.Vector v3 c
     , VG.Vector w3 c
     )
  => v1 a
  -> v2 b
  -> v3 c
  -> (w1 a, w2 b, w3 c)
convert3 v1 v2 v3 = (VG.convert v1, VG.convert v2, VG.convert v3)
