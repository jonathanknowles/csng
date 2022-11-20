module Algebra.Apportion.Natural
    ( CarryL (..)
    , CarryR (..)
    )
    where

import Algebra.Apportion
    ( Apportion (..), Apportionment (..), boundedApportionAsExact )
import Data.Monoid
    ( Sum (..) )
import Data.Monoid.Null
    ( MonoidNull (..), PositiveMonoid )
import Data.Traversable.Extended
    ( MapAccum (..), mapAccum )
import Data.Tuple
    ( swap )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (Quiet) )

import Prelude hiding
    ( last, null, splitAt, zip, zipWith )

-- | Apportions a natural number by carrying fractional remainders to the left.
--
newtype CarryL a = CarryL {getCarryL :: a}
    deriving stock (Eq, Ord, Generic)
    deriving (Semigroup, Monoid, MonoidNull, PositiveMonoid) via (Sum a)
    deriving (Read, Show) via Quiet (CarryL a)

-- | Apportions a natural number by carrying fractional remainders to the right.
--
newtype CarryR a = CarryR {getCarryR :: a}
    deriving stock (Eq, Ord, Generic)
    deriving (Semigroup, Monoid, MonoidNull, PositiveMonoid) via (Sum a)
    deriving (Read, Show) via Quiet (CarryL a)

instance Apportion (CarryL Natural) where
    type Weight (CarryL Natural) = CarryL Natural
    apportion a ws =
        CarryL <$> apportionNatural MapAccumR (getCarryL a) (getCarryL <$> ws)

instance Apportion (CarryR Natural) where
    type Weight (CarryR Natural) = CarryR Natural
    apportion a ws =
        CarryR <$> apportionNatural MapAccumL (getCarryR a) (getCarryR <$> ws)

apportionNatural
    :: Traversable t
    => MapAccum
    -> Natural
    -> t Natural
    -> Apportionment t Natural
apportionNatural acc a ws
    = snd
    $ mapAccum acc (fmap (swap . properFraction) . (+)) 0
    $ getSum <$> boundedApportionAsExact (Sum a) (Sum <$> ws)
