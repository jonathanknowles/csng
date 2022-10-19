{-# LANGUAGE UndecidableInstances #-}

module Value
    ( Balance
    , BalanceValue
    , Coin
    , CoinValue
    , FractionalCoin
    , FractionalCoinValue
    , HasAssets (..)
    , Assets (..)
    , Values (..)
    , coinToBalance
    , coinToFractionalCoin
    , balanceToCoins
    ) where

import Algebra.Apportion
    ( Apportion (..) )
import Algebra.Apportion.Balanced
    ( BalancedApportion (..) )
import AsList
    ( AsList (..), asList )
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
    , LeftReductive (..)
    , Reductive (..)
    , RightCancellative
    , RightReductive (..)
    )
import Data.Monoid.GCD
    ( OverlappingGCDMonoid (..) )
import Data.Monoid.Monus
    ( Monus (..) )
import Data.Monoid.Null
    ( MonoidNull, PositiveMonoid )
import Data.MonoidMap
    ( MonoidMap )
import Data.Ratio
    ( Ratio )
import Data.Set
    ( Set )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary )
import Test.QuickCheck.Instances.Natural
    ()
import Wrapped
    ( IsWrapped (..), Wrapped (..), wrapped )

import qualified Algebra.Apportion.Balanced as BalancedApportion
import qualified Data.MonoidMap as MonoidMap

--------------------------------------------------------------------------------
-- BalanceValue
--------------------------------------------------------------------------------

newtype BalanceValue = BalanceValue Integer
    deriving IsWrapped via Wrapped Integer
    deriving newtype (Arbitrary, Eq, FromInteger, Negatable, Ord, Read, Show)
    deriving
        ( Commutative
        , Group
        , LeftReductive
        , Monoid
        , MonoidNull
        , Reductive
        , RightReductive
        , Semigroup
        ) via Sum Integer

--------------------------------------------------------------------------------
-- CoinValue
--------------------------------------------------------------------------------

newtype CoinValue = CoinValue Natural
    deriving IsWrapped via Wrapped Natural
    deriving newtype (Arbitrary, Eq, FromInteger, Ord, Read, Show)
    deriving
        ( Apportion
        , Commutative
        , LeftReductive
        , Monoid
        , MonoidNull
        , Monus
        , OverlappingGCDMonoid
        , PositiveMonoid
        , Reductive
        , RightReductive
        , Semigroup
        ) via Sum Natural

--------------------------------------------------------------------------------
-- FractionalCoinValue
--------------------------------------------------------------------------------

newtype FractionalCoinValue = FractionalCoinValue (Ratio Natural)
    deriving IsWrapped via Wrapped (Ratio Natural)
    deriving newtype
        ( Arbitrary
        , Eq
        , FromFractional
        , FromInteger
        , Ord
        , Read
        , Show
        )
    deriving
        ( Commutative
        , Monoid
        , MonoidNull
        , PositiveMonoid
        , Semigroup
        ) via Sum (Ratio Natural)

instance Monus FractionalCoinValue where
   a <\> b = fromMaybe mempty (a </> b)

instance OverlappingGCDMonoid FractionalCoinValue where
    overlap (unwrap -> a) (unwrap -> b) = wrap $ min a b
    stripPrefixOverlap = flip (<\>)
    stripSuffixOverlap = flip (<\>)
    stripOverlap (unwrap -> a) (unwrap -> b) =
        ( wrap $ a - min a b
        , wrap $ min a b
        , wrap $ b - min a b
        )

instance Reductive FractionalCoinValue where
   a </> b
      | a > b = Just $ wrap $ unwrap a - unwrap b
      | otherwise = Nothing

instance LeftReductive FractionalCoinValue where
    stripPrefix = flip (</>)

instance RightReductive FractionalCoinValue where
    stripSuffix = flip (</>)

--------------------------------------------------------------------------------
-- HasAssets
--------------------------------------------------------------------------------

class HasAssets a where
    type Asset a
    type Value a
    filterAssets :: (Asset a -> Bool) -> a -> a
    getAssets :: a -> Set (Asset a)
    getAssetValue :: Ord (Asset a) => Asset a -> a -> Value a
    setAssetValue :: Ord (Asset a) => Asset a -> Value a -> a -> a
    singleton :: Asset a -> Value a -> a

newtype Assets a = Assets a
    deriving (Eq, Monoid, Semigroup, Show)

newtype Values a = Values a
    deriving (Eq, Monoid, Semigroup, Show)

--------------------------------------------------------------------------------
-- AssetValueMap
--------------------------------------------------------------------------------

newtype AssetValueMap a v = AssetValueMap (MonoidMap a v)
    deriving IsWrapped via Wrapped (MonoidMap a v)

instance (Ord a, MonoidNull v) => HasAssets (AssetValueMap a v) where
    type Asset (AssetValueMap a v) = a
    type Value (AssetValueMap a v) = v
    filterAssets f = wrapped . asList . filter $ f . fst
    getAssets = MonoidMap.keys . unwrap
    getAssetValue a = MonoidMap.get a . unwrap
    setAssetValue a = wrapped . MonoidMap.set a
    singleton a = wrap . MonoidMap.singleton a

--------------------------------------------------------------------------------
-- Balance
--------------------------------------------------------------------------------

newtype Balance a = Balance (MonoidMap a BalanceValue)
    deriving (Arbitrary, Read, Show) via AsList (Balance a)
    deriving HasAssets via AssetValueMap a BalanceValue
    deriving IsWrapped via Wrapped (MonoidMap a BalanceValue)
    deriving newtype
        ( Commutative
        , Eq
        , Group
        , IsList
        , Monoid
        , MonoidNull
        , Semigroup
        )

--------------------------------------------------------------------------------
-- Coin
--------------------------------------------------------------------------------

newtype Coin a = Coin (MonoidMap a CoinValue)
    deriving (Arbitrary, Read, Show) via AsList (Coin a)
    deriving HasAssets via AssetValueMap a CoinValue
    deriving IsWrapped via Wrapped (MonoidMap a CoinValue)
    deriving newtype
        ( Apportion
        , Cancellative
        , Commutative
        , Eq
        , IsList
        , LeftCancellative
        , LeftReductive
        , Monoid
        , MonoidNull
        , Monus
        , OverlappingGCDMonoid
        , PositiveMonoid
        , Reductive
        , RightCancellative
        , RightReductive
        , Semigroup
        )

deriving via BalancedApportion.Keys
    (MonoidMap a (Sum Natural))
    instance Ord a =>
    BalancedApportion (Assets (Coin a))

deriving via BalancedApportion.Values
    (MonoidMap a (Sum Natural))
    instance Ord a =>
    BalancedApportion (Values (Coin a))

--------------------------------------------------------------------------------
-- FractionalCoin
--------------------------------------------------------------------------------

newtype FractionalCoin a = FractionalCoin (MonoidMap a FractionalCoinValue)
    deriving (Arbitrary, Read, Show) via AsList (FractionalCoin a)
    deriving newtype
        ( Commutative
        , Eq
        , IsList
        , LeftReductive
        , Monoid
        , MonoidNull
        , Monus
        , OverlappingGCDMonoid
        , RightReductive
        , Semigroup
        )

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

balanceToCoins :: forall a. Ord a => Balance a -> (Coin a, Coin a)
balanceToCoins b = (toCoin (invert b), toCoin b)
  where
    toCoin :: Balance a -> Coin a
    toCoin = asList . fmap . fmap $ fromMaybe mempty . balanceValueToCoinValue

balanceValueToCoinValue :: BalanceValue -> Maybe CoinValue
balanceValueToCoinValue = fmap wrap . intCastMaybe . unwrap

coinToBalance :: Ord a => Coin a -> Balance a
coinToBalance = asList $ fmap $ fmap coinValueToBalanceValue

coinToFractionalCoin :: Ord a => Coin a -> FractionalCoin a
coinToFractionalCoin = asList $ fmap $ fmap coinValueToFractionalCoinValue

coinValueToBalanceValue :: CoinValue -> BalanceValue
coinValueToBalanceValue = wrapped intCast

coinValueToFractionalCoinValue :: CoinValue -> FractionalCoinValue
coinValueToFractionalCoinValue = wrapped (% 1)
