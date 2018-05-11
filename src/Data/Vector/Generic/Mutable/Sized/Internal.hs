{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS_HADDOCK not-home            #-}

module Data.Vector.Generic.Mutable.Sized.Internal
  ( MVector(..)
  ) where

import GHC.Generics (Generic)
import GHC.TypeLits
import Control.DeepSeq (NFData)
import Data.Data
import Foreign.Storable

-- | A wrapper to tag mutable vectors with a type level length.
--
-- Be careful when using the constructor here to not construct sized vectors
-- which have a different length than that specified in the type parameter!
newtype MVector v (n :: Nat) s a = MVector (v s a)
  deriving ( Generic, Typeable, Data, Storable, NFData )
