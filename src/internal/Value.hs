{-# LANGUAGE DeriveGeneric #-}
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
import Algebra.NewApportion
    ( Roundable (..) )
import AsList
    ( AsList (..), asList )
import Generic.Data
    ( Newtype, Old, pack, unpack )
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
import Data.Strict.Set
    ( Set )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary )
import Test.QuickCheck.Instances.Natural
    ()

import qualified Algebra.Apportion.Balanced as BalancedApportion
import qualified Data.MonoidMap as MonoidMap

--------------------------------------------------------------------------------
-- BalanceValue
--------------------------------------------------------------------------------

newtype BalanceValue = BalanceValue Integer
    deriving Generic
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
    deriving Generic
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
    deriving Generic
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
    overlap (unpack -> a) (unpack -> b) = pack $ min a b
    stripPrefixOverlap = flip (<\>)
    stripSuffixOverlap = flip (<\>)
    stripOverlap (unpack -> a) (unpack -> b) =
        ( pack $ a - min a b
        , pack $ min a b
        , pack $ b - min a b
        )

instance Reductive FractionalCoinValue where
   a </> b
      | a > b = Just $ pack $ unpack a - unpack b
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
    deriving newtype (Eq, Monoid, Semigroup, Show)

newtype Values a = Values a
    deriving newtype (Eq, Monoid, Semigroup, Show)

--------------------------------------------------------------------------------
-- AssetValueMap
--------------------------------------------------------------------------------

newtype AssetValueMap a v = AssetValueMap (MonoidMap a v)
    deriving Generic

instance (Ord a, MonoidNull v) => HasAssets (AssetValueMap a v) where
    type Asset (AssetValueMap a v) = a
    type Value (AssetValueMap a v) = v
    filterAssets f = unpacked . asList . filter $ f . fst
    getAssets = MonoidMap.nonNullKeys . unpack
    getAssetValue a = MonoidMap.get a . unpack
    setAssetValue a = unpacked . MonoidMap.set a
    singleton a = pack . MonoidMap.singleton a

--------------------------------------------------------------------------------
-- Balance
--------------------------------------------------------------------------------

newtype Balance a = Balance (MonoidMap a BalanceValue)
    deriving Generic
    deriving HasAssets via AssetValueMap a BalanceValue
    deriving (Arbitrary, Read, Show) via AsList (Balance a)
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
    deriving Generic
    deriving HasAssets via AssetValueMap a CoinValue
    deriving (Arbitrary, Read, Show) via AsList (Coin a)
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
    deriving Generic
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

instance Roundable FractionalCoinValue where
    type Rounded FractionalCoinValue = CoinValue
    roundUp = unpacked ceiling
    roundDown = unpacked floor

instance Ord a => Roundable (FractionalCoin a) where
    type Rounded (FractionalCoin a) = Coin a
    roundUp = unpacked $ MonoidMap.map roundUp
    roundDown = unpacked $ MonoidMap.map roundDown

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

unpacked :: (Newtype a, Newtype b) => (Old a -> Old b) -> a -> b
unpacked f = pack . f . unpack

balanceToCoins :: forall a. Ord a => Balance a -> (Coin a, Coin a)
balanceToCoins b = (toCoin (invert b), toCoin b)
  where
    toCoin :: Balance a -> Coin a
    toCoin = asList . fmap . fmap $ fromMaybe mempty . balanceValueToCoinValue

balanceValueToCoinValue :: BalanceValue -> Maybe CoinValue
balanceValueToCoinValue = fmap pack . intCastMaybe . unpack

coinToBalance :: Ord a => Coin a -> Balance a
coinToBalance = asList $ fmap $ fmap coinValueToBalanceValue

coinToFractionalCoin :: Ord a => Coin a -> FractionalCoin a
coinToFractionalCoin = asList $ fmap $ fmap coinValueToFractionalCoinValue

coinValueToBalanceValue :: CoinValue -> BalanceValue
coinValueToBalanceValue = unpacked intCast

coinValueToFractionalCoinValue :: CoinValue -> FractionalCoinValue
coinValueToFractionalCoinValue = unpacked (% 1)
