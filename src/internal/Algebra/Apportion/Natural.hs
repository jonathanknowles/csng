{- HLINT ignore "Use <$>" -}

module Algebra.Apportion.Natural
    ( SumL (..)
    , SumR (..)
    )
    where

import Algebra.Apportion
    ( Apportion (..)
    , Apportionment (..)
    , BoundedApportion (..)
    , ExactApportion
    , apportionMap
    , boundedApportionAsExact
    )
import Algebra.ExactBounded
    ( BoundedExact (..) )
import Algebra.PartialOrd
    ( PartialOrd )
import Data.Coerce
    ( Coercible, coerce )
import Data.Monoid
    ( Sum (..) )
import Data.Monoid.Null
    ( MonoidNull (..), PositiveMonoid )
import Data.Ratio
    ( Ratio )
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

newtype SumL a = SumL {getSumL :: a}
    deriving stock (Eq, Functor, Ord, Generic)
    deriving newtype (FromInteger, PartialOrd)
    deriving (Semigroup, Monoid, MonoidNull, PositiveMonoid) via Sum a
    deriving (Read, Show) via Quiet (SumL a)

newtype SumR a = SumR {getSumR :: a}
    deriving stock (Eq, Functor, Ord, Generic)
    deriving newtype (FromInteger, PartialOrd)
    deriving (Semigroup, Monoid, MonoidNull, PositiveMonoid) via Sum a
    deriving (Read, Show) via Quiet (SumL a)

instance Apportion (SumL Natural) where
    type Weight (SumL Natural) = SumL Natural
    apportion = apportionNatural MapAccumL

instance Apportion (SumR Natural) where
    type Weight (SumR Natural) = SumR Natural
    apportion = apportionNatural MapAccumR

instance Apportion (SumL (Ratio Natural)) where
    type Weight (SumL (Ratio Natural)) = SumL (Ratio Natural)
    apportion = apportionMap (SumL . getSum) (Sum . getSumL)

instance Apportion (SumR (Ratio Natural)) where
    type Weight (SumR (Ratio Natural)) = SumR (Ratio Natural)
    apportion = apportionMap (SumR . getSum) (Sum . getSumR)

instance BoundedApportion (SumL Natural) where
    type Exact (SumL Natural) = SumL (Ratio Natural)
instance BoundedApportion (SumR Natural) where
    type Exact (SumR Natural) = SumR (Ratio Natural)

instance ExactApportion (SumL (Ratio Natural))
instance ExactApportion (SumR (Ratio Natural))

instance BoundedExact (SumL Natural) (SumL (Ratio Natural)) where
    exact = fmap exact
    lowerBound = fmap lowerBound
    upperBound = fmap upperBound

instance BoundedExact (SumR Natural) (SumR (Ratio Natural)) where
    exact = fmap exact
    lowerBound = fmap lowerBound
    upperBound = fmap upperBound

apportionNatural
    :: forall t a. (Coercible a Natural, Traversable t)
    => MapAccum -> a -> t a -> Apportionment t a
apportionNatural mapAccumF a ws
    = fmap (coerce @Natural) $ snd
    $ mapAccum mapAccumF (fmap (swap . properFraction) . (+)) 0
    $ fmap (getSum @(Ratio Natural))
    $ boundedApportionAsExact (coerce @_ @(Sum Natural) a) (coerce <$> ws)
