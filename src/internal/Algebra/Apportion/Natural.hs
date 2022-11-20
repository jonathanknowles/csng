module Algebra.Apportion.Natural
    ( CarryL (..)
    , CarryR (..)
    )
    where

import Algebra.Apportion
    ( Apportion (..), boundedApportionAsExact )
import Data.Bifunctor
    ( bimap )
import Data.Monoid
    ( Sum (..) )
import Data.Monoid.Null
    ( MonoidNull (..), PositiveMonoid )
import Data.Ratio
    ( Ratio )
import Data.Traversable.Extended
    ( mapAccumL )
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

type NaturalRatio = Ratio Natural
type NaturalRatioSum = Sum NaturalRatio
type NaturalSum = Sum Natural

-- | Apportions a natural number by carrying fractional remainders to the left.
--
newtype CarryL a = CarryL {getCarryL :: a}
    deriving stock Generic
    deriving newtype (Eq, Ord, Semigroup, Monoid, MonoidNull, PositiveMonoid)
    deriving (Read, Show) via Quiet (CarryL a)

-- | Apportions a natural number by carrying fractional remainders to the right.
--
newtype CarryR a = CarryR {getCarryR :: a}
    deriving stock Generic
    deriving newtype (Eq, Ord, Semigroup, Monoid, MonoidNull, PositiveMonoid)
    deriving (Read, Show) via Quiet (CarryL a)

instance Apportion (CarryL NaturalSum) where
    type Weight (CarryL NaturalSum) = NaturalSum
    apportion a = fmap CarryL . carryL . boundedApportionAsExact (getCarryL a)

instance Apportion (CarryR NaturalSum) where
    type Weight (CarryR NaturalSum) = NaturalSum
    apportion a = fmap CarryR . carryR . boundedApportionAsExact (getCarryR a)

carryL :: Traversable s => s NaturalRatioSum -> s NaturalSum
carryL = snd . mapAccumL (fmap splitNaturalPart . (<>)) mempty

carryR :: Traversable s => s NaturalRatioSum -> s NaturalSum
carryR = snd . mapAccumL (fmap splitNaturalPart . (<>)) mempty

splitNaturalPart :: NaturalRatioSum -> (NaturalRatioSum, NaturalSum)
splitNaturalPart = bimap Sum Sum . swap . properFraction . getSum
