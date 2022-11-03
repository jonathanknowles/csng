{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.List.Fraction
    ( ListFraction
    , fromList
    , length
    , drop
    , take
    , splitAt
    )
    where

import Algebra.ExactBounded
    ( ExactBounded (..) )
import Data.Function
    ( on )
import Data.List
    ( groupBy )
import Data.Monoid
    ( Sum (..) )
import Data.Ratio
    ( Ratio )
import Data.Sized
    ( Sized (..) )
import Data.SizeDivisible
    ( SizeDivisible )
import Numeric.Natural
    ( Natural )

import Prelude hiding
    ( drop, fromList, length, splitAt, take )

import qualified Data.SizeDivisible as SD

newtype ListFraction a = ListFraction
    {getListFraction :: [(a, Ratio Natural)]}
    deriving (Eq, Show)

instance Eq a => Semigroup (ListFraction a) where
    ListFraction xs <> ListFraction ys = coalesce (ListFraction (xs <> ys))

instance Eq a => Monoid (ListFraction a) where
    mempty = ListFraction mempty

instance Eq a => ExactBounded (ListFraction a) [a] where
    toExact = fromList
    toLowerBound (ListFraction as) = do
        (a, n) <- fmap toLowerBound <$> as
        replicate (fromIntegral n) a
    toUpperBound (ListFraction as) = do
        (a, n) <- fmap toUpperBound <$> as
        replicate (fromIntegral n) a

instance Sized (ListFraction a) where
    type Size (ListFraction a) = Ratio Natural
    size = length

instance SizeDivisible (ListFraction a) where
    drop = drop
    take = take
    splitAt = splitAt

coalesce :: forall a. Eq a => ListFraction a -> ListFraction a
coalesce (ListFraction as) = ListFraction (labels `zip` totals)
  where
    groups :: [[(a, Ratio Natural)]]
    groups = groupBy ((==) `on` fst) as

    labels :: [a]
    labels = head . fmap fst <$> groups

    totals :: [Ratio Natural]
    totals = getSum . foldMap (Sum . snd) <$> groups

fromList :: Eq a => [a] -> ListFraction a
fromList = coalesce . ListFraction . fmap (, 1)

length :: ListFraction a -> Ratio Natural
length (ListFraction as) = getSum $ foldMap (Sum . snd) as

drop :: Ratio Natural -> ListFraction a -> ListFraction a
drop r0 (ListFraction f0) = ListFraction (dropInner r0 f0)
  where
    dropInner r ((a, s) : as)
        | r > s = dropInner (r - s) as
        | otherwise = (a, s - r) : as
    dropInner _ [] = []

take :: Ratio Natural -> ListFraction a -> ListFraction a
take r0 (ListFraction f0) = ListFraction (takeInner r0 f0)
  where
    takeInner r ((a, s) : as)
        | r > s = (a, s) : takeInner (r - s) as
        | otherwise = [(a, r)]
    takeInner _ [] = []

splitAt :: Ratio Natural -> ListFraction a -> (ListFraction a, ListFraction a)
splitAt r f = (take r f, drop r f)
