{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedLists #-}

module Change where

import Prelude hiding
    ( null )

import Algebra.NewApportion
    ( Apportionment (..), apportion )
import Data.Function
    ( on )
import Data.Monoid.Monus.Extended
    ( Monus (..), distance )
import Data.Monoid.Null
    ( MonoidNull (..) )
import Data.Semialign
    ( salign )
import Data.Traversable.Extended
    ( mapAccumSortedL )
import Value
    ( Coin, CoinValue, HasAssets (..) )

import qualified Data.Foldable as F
import qualified Data.List as L

makeChangeForCoin
    :: forall p c a v. (c ~ Coin a, v ~ CoinValue, Functor p, Ord a, Ord (p v))
    => (p v -> v)
    -> c
    -> [p c]
    -> (c, [p c])
makeChangeForCoin extract target weights =
    (remainder result, L.zipWith (<$) (partition result) weights)
  where
    result = apportion target $ selectWeightsForCoin extract target weights

selectWeightsForCoin
    :: forall p c a v. (c ~ Coin a, v ~ CoinValue, Functor p, Ord a, Ord (p v))
    => (p v -> v)
    -> c
    -> [p c]
    -> [c]
selectWeightsForCoin extract target weights =
    F.foldl' salign []
        (selectWeightsForAsset extract target weights <$> assets)
  where
    assets :: [a]
    assets = F.toList (getAssets target)

selectWeightsForAsset
    :: forall p c a v. (c ~ Coin a, v ~ CoinValue, Functor p, Ord a, Ord (p v))
    => (p v -> v)
    -> c
    -> [p c]
    -> a
    -> [c]
selectWeightsForAsset extract target weights asset =
    singleton asset <$> selectWeightsForCoinValue extract
        (getAssetValue asset target)
        ((fmap . fmap) (getAssetValue asset) weights)

selectWeightsForCoinValue
    :: forall p v. (v ~ CoinValue, Functor p, Ord (p v))
    => (p v -> v)
    -> v
    -> [p v]
    -> [v]
selectWeightsForCoinValue extract target weights =
    snd (mapAccumSortedL accum mempty weights)
  where
    accum :: v -> p v -> (v, v)
    accum v = takeUntilSumIsNonNullAndMinimalDistanceToTarget target v . extract

takeUntilSumIsNonNullAndMinimalDistanceToTarget
    :: (MonoidNull a, Monus a, Ord a) => a -> (a -> a -> (a, a))
takeUntilSumIsNonNullAndMinimalDistanceToTarget target sum0 a
    | null sum0 || distance target sum0 >= distance target sum1 =
        (sum1, a)
    | otherwise =
        (sum0, mempty)
  where
    sum1 = sum0 <> a

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

data Weight a = Input a | Output a
    deriving (Eq, Foldable, Functor, Show)

instance Ord a => Ord (Weight a) where
    compare = compare `on` weightPriority

newtype Priority = Priority Int
    deriving (Eq, Ord, Show)

weightPriority :: Weight a -> (Priority, a)
weightPriority = \case
    -- We give outputs a higher priority than inputs:
    Input  a -> (Priority 1, a)
    Output a -> (Priority 0, a)

weightValue :: Weight a -> a
weightValue = \case
    Input  a -> a
    Output a -> a

data AssetId = A | B | C | D | E
    deriving (Eq, Ord, Show)

example :: (Coin AssetId, [Weight (Coin AssetId)])
example = makeChangeForCoin weightValue target weights
  where
    target :: Coin AssetId
    target = [(A, 100), (B, 100), (C, 100), (D, 100), (E, 100)]

    weights :: [Weight (Coin AssetId)]
    weights = [weightA, weightB, weightC, weightD]

    weightA = Input  [(A, 100), (B,  50), (C,  30), (D,  20)]
    weightB = Input  [(A,  20), (B, 100), (C,  50), (D,  30)]
    weightC = Output [(A,  30), (B,  20), (C, 100), (D,  50)]
    weightD = Output [(A,  50), (B,  30), (C,  20), (D, 100)]
