{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Coin
    where

import Algebra.Equipartition
    ( Equipartition (..), Keys (..), Values (..) )
import Data.Group
    ( Group (..) )
import Data.IntCast
    ( intCast, intCastMaybe )
import Data.Maybe
    ( fromMaybe )
import Data.Monoid
    ( Sum (..) )
import Data.Monoid.Cancellative
    ( Cancellative
    , Commutative
    , LeftCancellative
    , LeftReductive
    , Reductive
    , RightCancellative
    , RightReductive
    )
import Data.Monoid.GCD
    ( OverlappingGCDMonoid )
import Data.Monoid.Monus
    ( Monus )
import Data.Monoid.Null
    ( MonoidNull, PositiveMonoid )
import Data.MonoidMap
    ( MonoidMap )
import Data.Strict.Set
    ( Set )
import GHC.Exts
    ( IsList (..) )
import Numeric.Natural
    ( Natural )
import Text.Read
    ( Read (..) )

import qualified Data.MonoidMap as MonoidMap

newtype Balance a = Balance {unBalance :: MonoidMap a (Sum Integer)}
    deriving stock Eq
    deriving (Read, Show) via (AsList (Balance a))
    deriving newtype (Commutative, Monoid, MonoidNull, Semigroup)
    deriving newtype (Cancellative, LeftCancellative, RightCancellative)
    deriving newtype (Reductive, LeftReductive, RightReductive)
    deriving newtype (Group)

newtype Coin a = Coin {unCoin :: MonoidMap a (Sum Natural)}
    deriving stock Eq
    deriving (Read, Show) via (AsList (Coin a))
    deriving newtype (Commutative, Monoid, MonoidNull, Semigroup)
    deriving newtype (Cancellative, LeftCancellative, RightCancellative)
    deriving newtype (Reductive, LeftReductive, RightReductive)
    deriving newtype (Monus, OverlappingGCDMonoid, PositiveMonoid)

newtype Assets a = Assets
    {unAssets :: a}
    deriving (Eq, Monoid, Semigroup, Show)

deriving via Keys (MonoidMap a (Sum Natural))
    instance Ord a => Equipartition (Assets (Coin a))

deriving via Values (MonoidMap a (Sum Natural))
    instance Ord a => Equipartition (Values (Coin a))

newtype AsList a = AsList {asList :: a}

instance (IsList a, Show (Item a)) => Show (AsList a) where
    show = show . toList . asList

instance (IsList a, Read (Item a)) => Read (AsList a) where
    readPrec = AsList . fromList <$> readPrec

class HasAssets f a where
    type Value f
    getAssets :: f a -> Set a
    getAssetValue :: Ord a => a -> f a -> Value f
    setAssetValue :: Ord a => a -> Value f -> f a -> f a

instance HasAssets Balance a where
    type Value Balance = Integer
    getAssets = MonoidMap.nonNullKeys . unBalance
    getAssetValue a = getSum . MonoidMap.get a . unBalance
    setAssetValue a q = Balance . MonoidMap.set a (Sum q) . unBalance

instance HasAssets Coin a where
    type Value Coin = Natural
    getAssets = MonoidMap.nonNullKeys . unCoin
    getAssetValue a = getSum . MonoidMap.get a . unCoin
    setAssetValue a q = Coin . MonoidMap.set a (Sum q) . unCoin

instance Ord a => IsList (Balance a) where
    type Item (Balance a) = (a, Integer)
    fromList = Balance . fromList . fmap (fmap Sum)
    toList = fmap (fmap getSum) . toList . unBalance

instance Ord a => IsList (Coin a) where
    type Item (Coin a) = (a, Natural)
    fromList = Coin . fromList . fmap (fmap Sum)
    toList = fmap (fmap getSum) . toList . unCoin

coinToBalance :: Ord a => Coin a -> Balance a
coinToBalance = Balance . MonoidMap.map (fmap intCast) . unCoin

balanceToCoins :: forall a. Ord a => Balance a -> (Coin a, Coin a)
balanceToCoins b = (balanceToCoin (invert b), balanceToCoin b)
  where
    balanceToCoin :: Balance a -> Coin a
    balanceToCoin
        = Coin
        . fromList
        . fmap (fmap (fmap (fromMaybe 0 . intCastMaybe)))
        . toList
        . unBalance
