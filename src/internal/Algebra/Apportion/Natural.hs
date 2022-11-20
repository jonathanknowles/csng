{- HLINT ignore "Use <$>" -}

module Algebra.Apportion.Natural
    ( CarryL (..)
    , CarryR (..)
    )
    where

import Algebra.Apportion
    ( Apportion (..), Apportionment (..), boundedApportionAsExact )
import Data.Coerce
    ( Coercible, coerce )
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

-- | Apportions a value by carrying fractional remainders to the left.
--
newtype CarryL a = CarryL {carryL :: a}
    deriving stock (Eq, Ord, Generic)
    deriving newtype FromInteger
    deriving (Semigroup, Monoid, MonoidNull, PositiveMonoid) via Sum a
    deriving (Read, Show) via Quiet (CarryL a)

-- | Apportions a value by carrying fractional remainders to the right.
--
newtype CarryR a = CarryR {carryR :: a}
    deriving stock (Eq, Ord, Generic)
    deriving newtype FromInteger
    deriving (Semigroup, Monoid, MonoidNull, PositiveMonoid) via Sum a
    deriving (Read, Show) via Quiet (CarryL a)

instance Apportion (CarryL Natural) where
    type Weight (CarryL Natural) = CarryL Natural
    apportion = apportionNatural MapAccumR

instance Apportion (CarryR Natural) where
    type Weight (CarryR Natural) = CarryR Natural
    apportion = apportionNatural MapAccumL

apportionNatural
    :: forall t a. (Coercible a Natural, Traversable t)
    => MapAccum -> a -> t a -> Apportionment t a
apportionNatural mapAccumF a ws
    = fmap (coerce @Natural)
    $ snd
    $ mapAccum mapAccumF (fmap (swap . properFraction) . (+)) 0
    $ fmap getSum
    $ boundedApportionAsExact @_ @(Sum Natural) (coerce a) (coerce <$> ws)
