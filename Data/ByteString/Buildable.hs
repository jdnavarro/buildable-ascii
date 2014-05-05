{-# LANGUAGE FlexibleInstances #-}
module Data.ByteString.Buildable where

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

class Buildable a where
    build :: a -> Builder

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
