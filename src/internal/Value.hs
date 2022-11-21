{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}

module Value
    ( Balance
    , BalanceValue
    , Coin
    , CoinFraction
    , CoinValue
    , CoinValueFraction
    , HasAssets (..)
    , Assets (..)
    , Values (..)
    , coinToBalance
    , coinToCoinFraction
    , balanceToCoins
    ) where

import Algebra.Apportion
    ( Apportion (..)
    , BoundedApportion (..)
    , CommutativeApportion
    , ExactApportion
    , apportionMap
    )
import Algebra.Apportion.MonoidMap
    ()
import Algebra.ExactBounded
    ( ExactBounded (..) )
import Algebra.PartialOrd.Extended
    ( PartialOrd )
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
    ( Ratio, (%) )
import Data.Set
    ( Set )
import Generic.Data
    ( Newtype, Old, pack, unpack )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary (..), choose, sized )
import Test.QuickCheck.Instances.Natural
    ()

import Prelude hiding
    ( (%) )

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
        ( Commutative
        , LeftReductive
        , Monoid
        , MonoidNull
        , Monus
        , PartialOrd
        , OverlappingGCDMonoid
        , PositiveMonoid
        , Reductive
        , RightReductive
        , Semigroup
        ) via Sum Natural

instance Apportion CoinValue where
    type Weight CoinValue = CoinValue
    apportion = apportionMap (pack . getSum) (Sum . unpack)

instance BoundedApportion CoinValue where
    type Exact CoinValue = CoinValueFraction

--------------------------------------------------------------------------------
-- CoinValueFraction
--------------------------------------------------------------------------------

newtype CoinValueFraction = CoinValueFraction (Ratio Natural)
    deriving Generic
    deriving newtype
        ( Eq
        , FromFractional
        , FromInteger
        , PartialOrd
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

instance Arbitrary CoinValueFraction where
    arbitrary = sized $ \n ->
        fmap CoinValueFraction $ (%)
            <$> fmap fromIntegral (choose (0, n))
            <*> fmap fromIntegral (choose (1, n))

instance Apportion CoinValueFraction where
    type Weight CoinValueFraction = CoinValueFraction
    apportion = apportionMap (pack . getSum) (Sum . unpack)

instance CommutativeApportion CoinValueFraction
instance ExactApportion CoinValueFraction

instance ExactBounded CoinValueFraction where
    type Bound CoinValueFraction = CoinValue
    exact = unpacked (% 1)
    lowerBound = unpacked floor
    upperBound = unpacked ceiling

instance Monus CoinValueFraction where
   a <\> b = fromMaybe mempty (a </> b)

instance OverlappingGCDMonoid CoinValueFraction where
    overlap (unpack -> a) (unpack -> b) = pack $ min a b
    stripPrefixOverlap = flip (<\>)
    stripSuffixOverlap = flip (<\>)
    stripOverlap (unpack -> a) (unpack -> b) =
        ( pack $ a - min a b
        , pack $ min a b
        , pack $ b - min a b
        )

instance Reductive CoinValueFraction where
   a </> b
      | a > b = Just $ pack $ unpack a - unpack b
      | otherwise = Nothing

instance LeftReductive CoinValueFraction where
    stripPrefix = flip (</>)

instance RightReductive CoinValueFraction where
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
    filterAssets = unpacked . MonoidMap.filterKeys
    getAssets = MonoidMap.keys . unpack
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

instance Foldable Balance where
    foldMap f (Balance a) = foldMap f (MonoidMap.keys a)

--------------------------------------------------------------------------------
-- Coin
--------------------------------------------------------------------------------

newtype Coin a = Coin (MonoidMap a CoinValue)
    deriving Generic
    deriving HasAssets via AssetValueMap a CoinValue
    deriving (Arbitrary, Read, Show) via AsList (Coin a)
    deriving newtype
        ( Cancellative
        , Commutative
        , Eq
        , IsList
        , LeftCancellative
        , LeftReductive
        , Monoid
        , MonoidNull
        , Monus
        , OverlappingGCDMonoid
        , PartialOrd
        , PositiveMonoid
        , Reductive
        , RightCancellative
        , RightReductive
        , Semigroup
        )

instance Ord a => Apportion (Coin a) where
    type Weight (Coin a) = Coin a
    apportion = apportionMap pack unpack

instance Ord a => BoundedApportion (Coin a) where
    type Exact (Coin a) = CoinFraction a

instance Foldable Coin where
    foldMap f (Coin a) = foldMap f (MonoidMap.keys a)

--------------------------------------------------------------------------------
-- CoinFraction
--------------------------------------------------------------------------------

newtype CoinFraction a = CoinFraction (MonoidMap a CoinValueFraction)
    deriving Generic
    deriving (Arbitrary, Read, Show) via AsList (CoinFraction a)
    deriving newtype
        ( Commutative
        , Eq
        , IsList
        , LeftReductive
        , Monoid
        , MonoidNull
        , Monus
        , OverlappingGCDMonoid
        , PartialOrd
        , PositiveMonoid
        , RightReductive
        , Semigroup
        )

instance Ord a => Apportion (CoinFraction a) where
    type Weight (CoinFraction a) = CoinFraction a
    apportion = apportionMap pack unpack

instance Ord a => CommutativeApportion (CoinFraction a)
instance Ord a => ExactApportion (CoinFraction a)

instance Ord a => ExactBounded (CoinFraction a) where
    type Bound (CoinFraction a) = Coin a
    exact = unpacked $ MonoidMap.mapValues exact
    lowerBound = unpacked $ MonoidMap.mapValues lowerBound
    upperBound = unpacked $ MonoidMap.mapValues upperBound

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

coinToCoinFraction :: Ord a => Coin a -> CoinFraction a
coinToCoinFraction = asList $ fmap $ fmap coinValueToCoinValueFraction

coinValueToBalanceValue :: CoinValue -> BalanceValue
coinValueToBalanceValue = unpacked intCast

coinValueToCoinValueFraction :: CoinValue -> CoinValueFraction
coinValueToCoinValueFraction = unpacked (% 1)
