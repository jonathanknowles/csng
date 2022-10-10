module Change where

import Prelude

import Algebra.Apportion
    ( Apportion (..) )
import Data.Bifunctor
    ( bimap )
import Data.Function
    ( on )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Monoid
    ( Sum (..) )
import Data.Monoid.Monus.Extended
    ( distance )
import Data.Ord
    ( Down (..) )
import Data.Semialign
    ( Semialign (..), salign )
import Data.Set
    ( Set )
import Data.These
    ( mergeThese )
import Numeric.Natural
    ( Natural )
import Value
    ( Coin, HasAssets (..) )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty.Extended as NE

makeChangeForCoin
    :: forall a p. (Ord a, Ord p)
    => (Coin a, NonEmpty (p, Coin a))
    -> (Coin a, NonEmpty (p, Coin a))
makeChangeForCoin (target, pws)
    = F.foldl' combine empty (changeForAsset <$> F.toList allAssets)
  where
    allAssets :: Set a
    allAssets = getAssets target <> F.foldMap getAssets (snd <$> pws)

    empty :: (Coin a, NonEmpty (p, Coin a))
    empty = (mempty, NE.zip (fst <$> pws) (NE.repeat mempty))

    combine
        :: (Coin a, NonEmpty (p, Coin a))
        -> (Coin a, NonEmpty (p, Coin a))
        -> (Coin a, NonEmpty (p, Coin a))
    combine (c0, pws0) (c1, pws1) =
        ( c0 <> c1
        , NE.zip (fst <$> pws) $ NE.zipWith (<>)
            (snd <$> pws0)
            (snd <$> pws1)
        )

    changeForAsset :: a -> (Coin a, NonEmpty (p, Coin a))
    changeForAsset a
        = bimap (singleton a) (fmap (fmap (singleton a)))
        $ makeChangeForAsset
            ( getAssetValue a target
            , fmap (getAssetValue a) <$> pws
            )

makeChangeForAsset
    :: forall p. Ord p
    => (Natural, NonEmpty (p, Natural))
    -> (Natural, NonEmpty (p, Natural))
makeChangeForAsset     (n,             pws)
    | weightSum == 0 = (n, fmap (0 <$) pws)
    | otherwise      = (0, result         )
  where
    weightSum = F.foldMap (Sum . snd) pws

    result :: NonEmpty (p, Natural)
    result
        = fmap (\((p, _), w) -> (p, w))
        . NE.sortWith (\((_, i), _) -> i)
        . NE.zip (fst <$> index)
        . alignWith (mergeThese (+)) (0 <$ pws)
        . snd
        . apportion n
        . takeUntilSumIsNonZeroAndMinimalDistanceToTarget n
        $ snd <$> index

    index :: NonEmpty ((p, Int), Natural)
    index
        = NE.sortWith (\((p, _), w) -> (p, Down w))
        $ NE.zip
            (NE.zip (fst <$> pws) (NE.iterate (+ 1) 0))
            (snd <$> pws)

takeUntilSumIsNonZeroAndMinimalDistanceToTarget
    :: Natural
    -> NonEmpty Natural
    -> NonEmpty Natural
takeUntilSumIsNonZeroAndMinimalDistanceToTarget target as =
    getSum <$> (salign `on` fmap Sum)
        (takeUntilSumIsNonZero as)
        (0 <$ takeUntilSumIsMinimalDistanceToTarget target as)

takeUntilSumIsNonZero
    :: NonEmpty Natural
    -> NonEmpty Natural
takeUntilSumIsNonZero = fst . NE.splitWhen (\a _ -> a > 0)

takeUntilSumIsMinimalDistanceToTarget
    :: Natural
    -> NonEmpty Natural
    -> NonEmpty Natural
takeUntilSumIsMinimalDistanceToTarget target as =
    fst <$> NE.zip as (fst $ NE.splitWhen (<=) distances)
  where
    distances = (distance `on` Sum) target <$> NE.scanl1 (+) as
