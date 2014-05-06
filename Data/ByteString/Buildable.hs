{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.ByteString.Buildable where

import GHC.Generics (Generic, Rep, from)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.ByteString.Builder
  ( Builder
  , byteString
  , lazyByteString
  , charUtf8
  , string8
  , intDec
  )

import Data.ByteString.Buildable.Generic

class Buildable a where
    build :: a -> Builder

    default build :: (Generic a, GBuildable (Rep a)) => a -> Builder
    build a = gbuild $ from a

instance Buildable Builder where
    build = id
    {-# INLINE build #-}

instance Buildable ByteString where
    build = byteString
    {-# INLINE build #-}

instance Buildable L.ByteString where
    build = lazyByteString
    {-# INLINE build #-}

instance Buildable Char where
    build = charUtf8
    {-# INLINE build #-}

instance Buildable String where
    build = string8
    {-# INLINE build #-}

instance Buildable Int where
    build = intDec
    {-# INLINE build #-}
