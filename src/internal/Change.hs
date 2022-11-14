{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedLists #-}

module Change where

import Prelude hiding
    ( null )

import Algebra.NewApportion
    ( Apportionment (..), apportion )
import Data.Function
    ( on )
import Data.Maybe
    ( mapMaybe )
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

data SelectionOf f g a = Selection
    { inputs  :: f (g a)
    , outputs :: f (g a)
    }
    deriving (Eq, Foldable, Show)

data WeightChange a = WeightChange
    { weight :: Coin a
    , change :: Coin a
    }
    deriving (Eq, Foldable, Show)

type Selection f a = SelectionOf f Coin a
type SelectionWithChange f a = SelectionOf f WeightChange a

makeChange
    :: forall a. Ord a
    => Selection [] a
    -> SelectionWithChange [] a
makeChange s@Selection {inputs, outputs} = Selection
    {  inputs = zipWith WeightChange  inputs (mapMaybe  input result)
    , outputs = zipWith WeightChange outputs (mapMaybe output result)
    }
  where
    result :: [Weight (Coin a)]
    result = snd $ makeChangeForCoin weightValue
        (selectionExcess s)
        (selectionWeights s)

selectionExcess :: (Foldable f, Ord a) => Selection f a -> Coin a
selectionExcess Selection {inputs, outputs} =
    F.fold inputs <\> F.fold outputs

selectionWeights :: Selection [] a -> [Weight (Coin a)]
selectionWeights Selection {inputs, outputs} = mconcat
    [  Input <$> F.toList  inputs
    , Output <$> F.toList outputs
    ]

--------------------------------------------------------------------------------
-- Internal
--------------------------------------------------------------------------------

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
    snd (mapAccumSortedL acc mempty weights)
  where
    acc :: v -> p v -> (v, v)
    acc v = takeUntilSumIsNonNullAndMinimalDistanceToTarget target v . extract

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

input :: Weight a -> Maybe a
input = \case
    Input  a -> Just a
    Output _ -> Nothing

output :: Weight a -> Maybe a
output = \case
    Input  _ -> Nothing
    Output a -> Just a

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

data ExampleAsset = A | B | C | D | E
    deriving (Eq, Ord, Show)

exampleSelection :: Selection [] ExampleAsset
exampleSelection = Selection
    { inputs =
        [ [(A, 32), (B, 20)         ]
        , [         (B, 20), (C, 20)]
        , [(A, 20),          (C, 20)]
        ]
    , outputs =
        [ [(A, 10), (B, 10)         ]
        , [         (B, 10), (C, 10)]
        , [(A, 10),          (C, 10)]
        ]
    }
