{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

module Data.Vector.Generic.Mutable.Sized.Internal (
    MVector(..)
  ) where

import GHC.Generics (Generic)
import GHC.TypeLits
import Data.Data

-- | A wrapper to tag mutable vectors with a type level length.
newtype MVector v (n :: Nat) s a = MVector (v s a)
  deriving ( Generic, Typeable, Data )

