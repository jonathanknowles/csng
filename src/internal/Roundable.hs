module Roundable where

import Data.Monoid
    ( Sum )
import Data.Ratio
    ( Ratio )
import Numeric.Natural
    ( Natural )

class ExactBounded e b | b -> e, e -> b where
    toExact :: b -> e
    toLowerBound :: e -> b
    toUpperBound :: e -> b

instance ExactBounded (Ratio Natural) Natural where
    toExact = (% 1)
    toLowerBound = floor
    toUpperBound = ceiling

instance ExactBounded (Sum (Ratio Natural)) (Sum Natural) where
    toExact = fmap (% 1)
    toLowerBound = fmap floor
    toUpperBound = fmap ceiling
