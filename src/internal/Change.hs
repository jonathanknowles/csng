module Change where

import Prelude hiding
    ( null )

import Algebra.NewApportion
    ( Apportion (..), Apportionment (..) )
import Data.Bifunctor
    ( bimap )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Monoid.Monus.Extended
    ( Monus (..), distance )
import Data.Monoid.Null
    ( MonoidNull (..) )
import Data.Ord
    ( Down (..) )
import Data.Semialign
    ( Semialign (..) )
import Data.Set
    ( Set )
import Data.These
    ( mergeThese )
import Data.Traversable.Extended
    ( mapTraverseNonEmpty )
import Value
    ( Coin, CoinValue, HasAssets (..) )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty.Extended as NE

-- 1. assign priorities
-- 2. zero out low priorities
-- 3. apportion

-- Make a function that zeros out low-priority weights from within Coins.
-- Then use the ordinary apportion functions for Coin.
--
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
    => (CoinValue, NonEmpty (p, CoinValue))
    -> (CoinValue, NonEmpty (p, CoinValue))
makeChangeForAsset     (n,                       pws)
    | null weightSum = (n     , fmap (mempty <$) pws)
    | otherwise      = (mempty, result              )
  where
    weightSum = F.foldMap snd pws

    result :: NonEmpty (p, CoinValue)
    result
        = fmap (\((p, _), w) -> (p, w))
        . NE.sortWith (\((_, i), _) -> i)
        . NE.zip (fst <$> index)
        . alignWith (mergeThese (<>)) (mempty <$ pws)
        . partition
        . apportion n
        . nullifyAfterSumIsNonNullAndMinimalDistanceToTarget n
        $ snd <$> index

    index :: NonEmpty ((p, Int), CoinValue)
    index
        = NE.sortWith (\((p, _), w) -> (p, Down w))
        $ NE.zip
            (NE.zip (fst <$> pws) (NE.iterate (+ 1) 0))
            (snd <$> pws)

nullifyAfterSumIsNonNullAndMinimalDistanceToTarget
    :: (MonoidNull a, Monus a, Ord a, Traversable t)
    => a
    -> t a
    -> t a
nullifyAfterSumIsNonNullAndMinimalDistanceToTarget target =
    mapTraverseNonEmpty $ \as ->
    alignWith (mergeThese const)
        (takeUntilSumIsNonNull as)
        (takeUntilSumIsMinimalDistanceToTarget target as)

takeUntilSumIsNonNull
    :: MonoidNull a
    => NonEmpty a
    -> NonEmpty a
takeUntilSumIsNonNull = fst . NE.splitWhen (\a _ -> not (null a))

takeUntilSumIsMinimalDistanceToTarget
    :: (Monus a, Ord a)
    => a
    -> NonEmpty a
    -> NonEmpty a
takeUntilSumIsMinimalDistanceToTarget target as =
    fst <$> NE.zip as (fst $ NE.splitWhen (<=) distances)
  where
    distances = distance target <$> NE.scanl1 (<>) as
