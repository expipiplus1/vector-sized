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
  ) where

import           Control.DeepSeq      (NFData)
import           Data.Data
import           Data.Functor.Classes
import qualified Data.Vector as V     (Vector, and, empty, foldl', fromList,
                                      null, toList, zipWith, zipWith3)
import           Foreign.Storable
import           GHC.Arr              (Ix (inRange, range, unsafeIndex,
                                      unsafeRangeSize))
import           GHC.Generics         (Generic)
import           GHC.TypeLits

-- | A wrapper to tag vectors with a type level length.
--
-- Be careful when using the constructor here to not construct sized vectors
-- which have a different length than that specified in the type parameter!
newtype Vector v (n :: Nat) a = Vector (v a)
  deriving ( Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic
           , Show1, Eq1, Ord1
           , Data, Typeable
           )

instance (Ix a) => Ix (Vector V.Vector n a) where

  -- range is consistent with range :: ((a,..,a), (a,..,a)) -> [(a,..,a)]
  range (Vector l, Vector u) = Vector <$> enumerate ranges
   where
    ranges = V.zipWith (curry range) l u
    enumerate v
      | V.null v = [V.empty]
      | otherwise = map V.fromList $ enumerate' (V.toList v)
    enumerate' [] = [[]]
    enumerate' (xs:xss) = [ x : xs' | x <- xs, xs' <- enumerate' xss ]

  -- index/unsafeIndex is consistent with
  -- index :: ((a,..,a), (a,..,a)) -> (a,..,a) -> Int
  unsafeIndex (Vector l, Vector u) (Vector i) = V.foldl' f 0 v
   where
    f acc (index', rangeSize') = acc * rangeSize' + index'
    v = V.zipWith3 indexAndRangeSize l u i
    indexAndRangeSize l' u' i' = let b' = (l', u')
                                 in  (unsafeIndex b' i', unsafeRangeSize b')

  -- i is in range (l, u) if, and only if, that is true for all elements,
  -- element-by-element
  inRange (Vector l, Vector u) (Vector i) =
    V.and $ V.zipWith3 (curry inRange) l u i
