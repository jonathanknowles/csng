{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Value where

import Algebra.Equipartition
    ( Equipartition (..), Keys (..), Values (..) )
import AsList
    ( AsList (..) )
import Data.Group
    ( Group (..) )
import Data.IntCast
    ( intCast, intCastMaybe )
import Data.Maybe
    ( fromMaybe )
import Data.Monoid
    ( Sum (..) )
import Data.Monoid.Cancellative
    ( Commutative, LeftReductive, Reductive, RightReductive, SumCancellative )
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

import qualified Data.MonoidMap as MonoidMap

--------------------------------------------------------------------------------
-- AssetValueMap
--------------------------------------------------------------------------------

newtype AssetValueMap a i = AssetValueMap
    {unAssetValueMap :: MonoidMap a (Sum i)}
    deriving stock Eq
    deriving (Read, Show) via (AsList (AssetValueMap a i))
    deriving newtype (Commutative, Monoid, MonoidNull, Semigroup)
    deriving newtype (Reductive, LeftReductive, RightReductive)
    deriving newtype (PositiveMonoid)
    deriving newtype (Group)

instance (Ord a, Eq i, Num i) => IsList (AssetValueMap a i) where
    type Item (AssetValueMap a i) = (a, i)
    fromList = AssetValueMap . fromList . fmap (fmap Sum)
    toList = fmap (fmap getSum) . toList . unAssetValueMap

type Deriving c a i = (Ord a, Eq i, Num i, c (Sum i), SumCancellative i)

deriving instance Deriving
    Monus a i =>
    Monus (AssetValueMap a i)
deriving instance Deriving
    OverlappingGCDMonoid a i =>
    OverlappingGCDMonoid (AssetValueMap a i)

class HasAssets a where
    type Asset a
    type Value a
    getAssets :: a -> Set (Asset a)
    getAssetValue :: Ord a => Asset a -> a -> Value a
    setAssetValue :: Ord a => Asset a -> Value a -> a -> a

instance (Ord a, Eq i, Num i) => HasAssets (AssetValueMap a i) where
    type Asset (AssetValueMap a i) = a
    type Value (AssetValueMap a i) = i
    getAssets = MonoidMap.nonNullKeys . unAssetValueMap
    getAssetValue a = getSum . MonoidMap.get a . unAssetValueMap
    setAssetValue a q = AssetValueMap . MonoidMap.set a (Sum q) . unAssetValueMap

--------------------------------------------------------------------------------
-- Balance
--------------------------------------------------------------------------------

newtype Balance a = Balance {unBalance :: AssetValueMap a Integer}
    deriving stock Eq
    deriving newtype (IsList)
    deriving newtype (Read, Show)
    deriving newtype (Commutative, Monoid, MonoidNull, Semigroup)
    deriving newtype (Group)

--------------------------------------------------------------------------------
-- Coin
--------------------------------------------------------------------------------

newtype Coin a = Coin {unCoin :: AssetValueMap a Natural}
    deriving stock Eq
    deriving newtype (IsList)
    deriving newtype (Read, Show)
    deriving newtype (Commutative, Monoid, MonoidNull, Semigroup)
    deriving newtype (Reductive, LeftReductive, RightReductive)
    deriving newtype (Monus, OverlappingGCDMonoid, PositiveMonoid)

newtype Assets a = Assets
    {unAssets :: a}
    deriving (Eq, Monoid, Semigroup, Show)

deriving via Keys (MonoidMap a (Sum Natural))
    instance Ord a => Equipartition (Assets (Coin a))

deriving via Values (MonoidMap a (Sum Natural))
    instance Ord a => Equipartition (Values (Coin a))

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

coinToBalance :: Ord a => Coin a -> Balance a
coinToBalance = fromList . fmap (fmap intCast) . toList

balanceToCoins :: forall a. Ord a => Balance a -> (Coin a, Coin a)
balanceToCoins b = (balanceToCoin (invert b), balanceToCoin b)
  where
    balanceToCoin :: Balance a -> Coin a
    balanceToCoin = fromList . fmap (fmap (fromMaybe 0 . intCastMaybe)) . toList
