{-# LANGUAGE TypeFamilyDependencies #-}

module Roundable where

import Data.Monoid
    ( Sum )
import Data.Ratio
    ( Ratio )
import Numeric.Natural
    ( Natural )

class HasFraction a where
    type Fraction a = f | f -> a
    toFraction :: a -> Fraction a
    lowerBound :: Fraction a -> a
    upperBound :: Fraction a -> a

instance HasFraction Natural where
    type Fraction Natural = Ratio Natural
    toFraction = (% 1)
    lowerBound = floor
    upperBound = ceiling

instance HasFraction (Sum Natural) where
    type Fraction (Sum Natural) = Sum (Ratio Natural)
    toFraction = fmap (% 1)
    lowerBound = fmap floor
    upperBound = fmap ceiling
