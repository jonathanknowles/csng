{-# LANGUAGE OverloadedLists #-}

module Change where

import Prelude hiding
    ( null )

import Algebra.NewApportion
    ( Apportionment (..), apportion )
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

makeChangeForCoin
    :: forall p c a v. (c ~ Coin a, v ~ CoinValue, Functor p, Ord a, Ord (p v))
    => (p v -> v)
    -> c
    -> [p c]
    -> Apportionment [] c
makeChangeForCoin extract target weights =
    apportion target $ selectWeightsForCoin extract target weights

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
    :: forall p v. (v ~ CoinValue, Ord (p v), Functor p)
    => (p v -> v)
    -> v
    -> [p v]
    -> [v]
selectWeightsForCoinValue extract target weights =
    fmap extract $ snd $ mapAccumSortedL
        extract
        (takeUntilSumIsNonNullAndMinimalDistanceToTarget target)
        mempty
        weights

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

data Weight a
    = Input a
    | Output a
    deriving (Eq, Functor, Ord, Show)

extractWeight :: Weight a -> a
extractWeight = \case
    Input a -> a
    Output a -> a

data AssetId = A | B | C | D
    deriving (Eq, Ord, Show)

example :: Apportionment [] (Coin AssetId)
example = makeChangeForCoin extractWeight target weights
  where
    target :: Coin AssetId
    target = [(A, 200), (B, 200), (C, 200), (D, 200)]

    weights :: [Weight (Coin AssetId)]
    weights = [weightA, weightB, weightC, weightD]

    weightA = Input  [(A, 100), (B,   0), (C,   0), (D,   0)]
    weightB = Input  [(A,   0), (B, 100), (C,   0), (D,   0)]
    weightC = Output [(A,   0), (B,   0), (C, 100), (D,   0)]
    weightD = Output [(A,   0), (B,   0), (C,   0), (D, 100)]
