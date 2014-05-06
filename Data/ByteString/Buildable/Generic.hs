{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.ByteString.Buildable.Generic where

import Data.Monoid ((<>))
import GHC.Generics (Constructor, D1, C1, (:+:), from)
import Data.ByteString.Builder (Builder)

class GBuildable f where
    gbuild :: f a -> Builder

instance (GBuildable f, GBuildable g) => GBuildable (f :+: g) where
    gbuild (_ :: (f :+: g) a) = gbuild (undefined :: f a)
                             <> gbuild (undefined :: g a)

instance (GBuildable f) => GBuildable (D1 c f) where
    gbuild (_ :: (D1 c f) a) = gbuild (undefined :: f a)

instance (Constructor c) => GBuildable (C1 c f) where
    gbuild x = gbuild $ undefined `asTypeOf` from x
