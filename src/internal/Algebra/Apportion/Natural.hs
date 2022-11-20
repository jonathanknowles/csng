{- HLINT ignore "Use <$>" -}

module Algebra.Apportion.Natural
    ( SumL (..)
    , SumR (..)
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

newtype SumL a = SumL {sumL :: a}
    deriving stock (Eq, Ord, Generic)
    deriving newtype FromInteger
    deriving (Semigroup, Monoid, MonoidNull, PositiveMonoid) via Sum a
    deriving (Read, Show) via Quiet (SumL a)

newtype SumR a = SumR {sumR :: a}
    deriving stock (Eq, Ord, Generic)
    deriving newtype FromInteger
    deriving (Semigroup, Monoid, MonoidNull, PositiveMonoid) via Sum a
    deriving (Read, Show) via Quiet (SumL a)

instance Apportion (SumL Natural) where
    type Weight (SumL Natural) = SumL Natural
    apportion = apportionNatural MapAccumL

instance Apportion (SumR Natural) where
    type Weight (SumR Natural) = SumR Natural
    apportion = apportionNatural MapAccumR

apportionNatural
    :: forall t a. (Coercible a Natural, Traversable t)
    => MapAccum -> a -> t a -> Apportionment t a
apportionNatural mapAccumF a ws
    = fmap (coerce @Natural)
    $ snd
    $ mapAccum mapAccumF (fmap (swap . properFraction) . (+)) 0
    $ fmap getSum
    $ boundedApportionAsExact @_ @(Sum Natural) (coerce a) (coerce <$> ws)
