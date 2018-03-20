{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}

module Data.Vector.Generic.Sized.Internal
  ( Vector(..)
  ) where

import           Control.DeepSeq      (NFData)
import           Data.Data
import           Data.Functor.Classes
import           Foreign.Storable
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
