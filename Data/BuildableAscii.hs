module Data.BuildableAscii
  ( BuildableAscii(..)
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

class BuildableAscii a where
    build :: a -> Builder

instance BuildableAscii Builder where
    build = id
    {-# INLINE build #-}

instance BuildableAscii ByteString where
    build = byteString
    {-# INLINE build #-}

instance BuildableAscii L.ByteString where
    build = lazyByteString
    {-# INLINE build #-}

instance BuildableAscii Int where
    build = intDec
    {-# INLINE build #-}

instance BuildableAscii Int8 where
    build = int8Dec
    {-# INLINE build #-}

instance BuildableAscii Int16 where
    build = int16Dec
    {-# INLINE build #-}

instance BuildableAscii Int32 where
    build = int32Dec
    {-# INLINE build #-}

instance BuildableAscii Int64 where
    build = int64Dec
    {-# INLINE build #-}

instance BuildableAscii Word where
    build = wordDec
    {-# INLINE build #-}

instance BuildableAscii Word8 where
    build = word8Dec
    {-# INLINE build #-}

instance BuildableAscii Word16 where
    build = word16Dec
    {-# INLINE build #-}

instance BuildableAscii Word32 where
    build = word32Dec
    {-# INLINE build #-}

instance BuildableAscii Word64 where
    build = word64Dec
    {-# INLINE build #-}

instance BuildableAscii Integer where
    build = integerDec
    {-# INLINE build #-}

instance BuildableAscii Float where
    build = floatDec
    {-# INLINE build #-}

instance BuildableAscii Double where
    build = doubleDec
    {-# INLINE build #-}

instance BuildableAscii a => BuildableAscii (Maybe a) where
    build Nothing  = mempty
    build (Just v) = build v
    {-# INLINE build #-}

toByteString :: BuildableAscii a => a -> ByteString
toByteString = toStrict . toLazyByteString . build
