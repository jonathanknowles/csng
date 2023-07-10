module Wrapped where

import Prelude

import Data.Coerce
    ( Coercible, coerce )

-- Use this
-- https://hackage.haskell.org/package/generic-data-1.0.0.0/docs/src/Generic.Data.Internal.Newtype.html#Newtype
--
newtype Wrap t = Wrap t

class Wrapped a where
    type Unwrapped a

    wrap :: Unwrapped a -> a
    default wrap :: Coercible (Unwrapped a) a => Unwrapped a -> a
    wrap = coerce

    unwrap :: a -> Unwrapped a
    default unwrap :: Coercible a (Unwrapped a) => a -> Unwrapped a
    unwrap = coerce

instance Wrapped (Wrap t) where
    type Unwrapped (Wrap t) = t

wrapped :: (Wrapped a, Wrapped b) => (Unwrapped a -> Unwrapped b) -> a -> b
wrapped f = wrap . f . unwrap
