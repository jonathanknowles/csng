module Roundable where

import Data.Monoid
    ( Sum )
import Data.Ratio
    ( Ratio )
import Numeric.Natural
    ( Natural )

class Roundable a where
    type Rounded a
    roundDown :: a -> Rounded a
    roundUp :: a -> Rounded a

instance Roundable (Ratio Natural) where
    type Rounded (Ratio Natural) = Natural
    roundDown = floor
    roundUp = ceiling

instance Roundable (Sum (Ratio Natural)) where
    type Rounded (Sum (Ratio Natural)) = Sum Natural
    roundDown = fmap floor
    roundUp = fmap ceiling
