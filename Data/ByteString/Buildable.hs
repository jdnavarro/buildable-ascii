{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.ByteString.Buildable
  ( Buildable(..)
  , toByteString
  , toLazyByteString
  ) where

import Data.Int
  ( Int8
  , Int16
  , Int32
  , Int64
  )
import Data.Word
  ( Word
  , Word8
  , Word16
  , Word32
  , Word64
  )
import Data.Monoid (mempty)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.ByteString.Builder
  ( Builder
  , byteString
  , lazyByteString
  , toLazyByteString
  , char7
  , string7
  , intDec
  , int8Dec
  , int16Dec
  , int32Dec
  , int64Dec
  , wordDec
  , word8Dec
  , word16Dec
  , word32Dec
  , word64Dec
  , integerDec
  , floatDec
  , doubleDec
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
    build = char7
    {-# INLINE build #-}

instance Buildable String where
    build = string7
    {-# INLINE build #-}

instance Buildable Int where
    build = intDec
    {-# INLINE build #-}

instance Buildable Int8 where
    build = int8Dec
    {-# INLINE build #-}

instance Buildable Int16 where
    build = int16Dec
    {-# INLINE build #-}

instance Buildable Int32 where
    build = int32Dec
    {-# INLINE build #-}

instance Buildable Int64 where
    build = int64Dec
    {-# INLINE build #-}

instance Buildable Word where
    build = wordDec
    {-# INLINE build #-}

instance Buildable Word8 where
    build = word8Dec
    {-# INLINE build #-}

instance Buildable Word16 where
    build = word16Dec
    {-# INLINE build #-}

instance Buildable Word32 where
    build = word32Dec
    {-# INLINE build #-}

instance Buildable Word64 where
    build = word64Dec
    {-# INLINE build #-}

instance Buildable Integer where
    build = integerDec
    {-# INLINE build #-}

instance Buildable Float where
    build = floatDec
    {-# INLINE build #-}

instance Buildable Double where
    build = doubleDec
    {-# INLINE build #-}

instance Buildable a => Buildable (Maybe a) where
    build Nothing  = mempty
    build (Just v) = build v


toByteString :: Buildable a => a -> ByteString
toByteString = toStrict . toLazyByteString . build
