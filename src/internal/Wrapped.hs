{-# LANGUAGE DefaultSignatures #-}

module Wrapped where

import Prelude

import Data.Coerce
    ( Coercible, coerce )

newtype Wrapped t = Wrapped t

instance IsWrapped (Wrapped t) where
    type Unwrapped (Wrapped t) = t

class IsWrapped a where
    type Unwrapped a

    wrap :: Unwrapped a -> a
    default wrap :: Coercible (Unwrapped a) a => Unwrapped a -> a
    wrap = coerce

    unwrap :: a -> Unwrapped a
    default unwrap :: Coercible a (Unwrapped a) => a -> Unwrapped a
    unwrap = coerce

wrapped :: (IsWrapped a, IsWrapped b) => (Unwrapped a -> Unwrapped b) -> a -> b
wrapped f = wrap . f . unwrap
