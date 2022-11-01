module Roundable where

import Data.Monoid
    ( Sum )
import Data.Ratio
    ( Ratio )
import Numeric.Natural
    ( Natural )

import Prelude hiding (Fractional)

class Fractional a f | a -> f, f -> a where
    toFraction :: a -> f
    lowerBound :: f -> a
    upperBound :: f -> a

instance Fractional Natural (Ratio Natural) where
    toFraction = (% 1)
    lowerBound = floor
    upperBound = ceiling

instance Fractional (Sum Natural) (Sum (Ratio Natural)) where
    toFraction = fmap (% 1)
    lowerBound = fmap floor
    upperBound = fmap ceiling
