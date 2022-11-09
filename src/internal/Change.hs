{-# LANGUAGE OverloadedLists #-}

module Change where

import Prelude hiding
    ( null )

import Algebra.NewApportion
    ( Apportionment (..), apportion )
import Data.Function
    ( on )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Monoid.Monus.Extended
    ( Monus (..), distance )
import Data.Monoid.Null
    ( MonoidNull (..) )
import Data.Semialign
    ( Semialign (..), salign )
import Data.These
    ( mergeThese )
import Data.Traversable.Extended
    ( mapTraverseNonEmpty )
import Value
    ( Coin, CoinValue, HasAssets (..) )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty.Extended as NE

makeChangeForCoin
    :: forall p c a v. (c ~ Coin a, v ~ CoinValue, Functor p, Ord a)
    => (p v -> p v -> Ordering)
    -> (p v -> v)
    -> c
    -> [p c]
    -> Apportionment [] c
makeChangeForCoin order extract target weights =
    apportion target $ selectWeightsForCoin order extract target weights

selectWeightsForCoin
    :: forall p c a v. (c ~ Coin a, v ~ CoinValue, Functor p, Ord a)
    => (p v -> p v -> Ordering)
    -> (p v -> v)
    -> c
    -> [p c]
    -> [c]
selectWeightsForCoin order extract target weights =
    F.foldl' salign []
        (selectWeightsForAsset order extract target weights <$> assets)
  where
    assets :: [a]
    assets = F.toList (getAssets target)

selectWeightsForAsset
    :: forall p c a v. (c ~ Coin a, v ~ CoinValue, Functor p, Ord a)
    => (p v -> p v -> Ordering)
    -> (p v -> v)
    -> c
    -> [p c]
    -> a
    -> [c]
selectWeightsForAsset order extract target weights asset =
    singleton asset <$> selectWeightsForCoinValue order extract
        (getAssetValue asset target)
        ((fmap . fmap) (getAssetValue asset) weights)

selectWeightsForCoinValue
    :: forall p v. (v ~ CoinValue)
    => (p v -> p v -> Ordering)
    -> (p v -> v)
    -> v
    -> [p v]
    -> [v]
selectWeightsForCoinValue order extract target weights
    = index
    & fmap snd
    & selectWeightsForCoinValueOrdered target
    & L.zip (fst <$> index)
    & L.sortOn fst
    & fmap snd
  where
    index :: [(Int, v)]
    index
        = weights
        & L.zip (L.iterate (+ 1) 0)
        & L.sortBy (order `on` snd)
        & fmap (fmap extract)

selectWeightsForCoinValueOrdered
    :: (v ~ CoinValue) => v -> [v] -> [v]
selectWeightsForCoinValueOrdered target =
    mapTraverseNonEmpty $ \as ->
    alignWith (mergeThese const)
        (takeUntilSumIsNonNull as)
        (takeUntilSumIsMinimalDistanceToTarget target as)

takeUntilSumIsNonNull
    :: MonoidNull a => NonEmpty a -> NonEmpty a
takeUntilSumIsNonNull = fst . NE.splitWhen (\a _ -> not (null a))

takeUntilSumIsMinimalDistanceToTarget
    :: (Monus a, Ord a) => a -> NonEmpty a -> NonEmpty a
takeUntilSumIsMinimalDistanceToTarget target as =
    fst <$> NE.zip as (fst $ NE.splitWhen (<=) distances)
  where
    distances = distance target <$> NE.scanl1 (<>) as

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
example = makeChangeForCoin compare extractWeight target weights
  where
    target :: Coin AssetId
    target = [(A, 200), (B, 200), (C, 200), (D, 200)]

    weights :: [Weight (Coin AssetId)]
    weights = [weightA, weightB, weightC, weightD]

    weightA = Input  [(A, 100), (B,   0), (C,   0), (D,   0)]
    weightB = Input  [(A,   0), (B, 100), (C,   0), (D,   0)]
    weightC = Output [(A,   0), (B,   0), (C, 100), (D,   0)]
    weightD = Output [(A,   0), (B,   0), (C,   0), (D, 100)]
