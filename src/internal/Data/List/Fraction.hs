{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use list literal pattern" -}

module Data.List.Fraction
    ( ListFraction
    , fromFractions
    , fromList
    , length
    , drop
    , take
    , splitAt
    , isInfixOf
    , isPrefixOf
    , isSuffixOf
    , isValid
    )
    where

import Algebra.ExactBounded
    ( ExactBounded (..) )
import Algebra.PartialOrd.Extended
    ( Infix (..), PartialOrd (..), Prefix (..), Suffix (..) )
import Data.Function
    ( on )
import Data.List
    ( groupBy )
import Data.Monoid
    ( Sum (..) )
import Data.Monoid.Monus.Extended
    ( (<\>) )
import Data.Monoid.Null
    ( MonoidNull (..), PositiveMonoid )
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
    deriving stock (Eq, Show, Read)
    deriving newtype (MonoidNull, PositiveMonoid)

instance Eq a => Semigroup (ListFraction a) where
    ListFraction xs <> ListFraction ys = toCanonical (ListFraction (xs <> ys))

instance Eq a => Monoid (ListFraction a) where
    mempty = ListFraction mempty

instance Eq a => ExactBounded (Infix (ListFraction a)) (Infix [a]) where
    exact (Infix f) = Infix (fromList f)
    lowerBound (Infix (ListFraction as)) = Infix $ do
        (a, n) <- fmap lowerBound <$> as
        replicate (fromIntegral n) a
    upperBound (Infix (ListFraction as)) = Infix $ do
        (a, n) <- fmap upperBound <$> as
        replicate (fromIntegral n) a

instance Sized (ListFraction a) where
    type Size (ListFraction a) = Ratio Natural
    size = length

instance SizeDivisible (ListFraction a) where
    drop = drop
    take = take
    splitAt = splitAt

instance Eq a => PartialOrd (Prefix (ListFraction a)) where
    Prefix f1 `leq` Prefix f2 = f1 `isPrefixOf` f2

instance Eq a => PartialOrd (Suffix (ListFraction a)) where
    Suffix f1 `leq` Suffix f2 = f1 `isSuffixOf` f2

instance Eq a => PartialOrd (Infix (ListFraction a)) where
    Infix f1 `leq` Infix f2 = f1 `isInfixOf` f2

fromFractions :: Eq a => [(a, Ratio Natural)] -> ListFraction a
fromFractions fs = toCanonical (ListFraction fs)

fromList :: Eq a => [a] -> ListFraction a
fromList = fromFractions . fmap (, 1)

toCanonical :: forall a. Eq a => ListFraction a -> ListFraction a
toCanonical (ListFraction as) =
    ListFraction (filter ((/= 0) . snd) $ labels `zip` totals)
  where
    groups :: [[(a, Ratio Natural)]]
    groups = groupBy ((==) `on` fst) as

    labels :: [a]
    labels = head . fmap fst <$> groups

    totals :: [Ratio Natural]
    totals = getSum . foldMap (Sum . snd) <$> groups

length :: ListFraction a -> Ratio Natural
length (ListFraction as) = getSum $ foldMap (Sum . snd) as

drop :: Ratio Natural -> ListFraction a -> ListFraction a
drop r0 (ListFraction f0) = ListFraction (dropInner r0 f0)
  where
    dropInner r ((a, s) : as)
        | r == 0 = (a, s) : as
        | r == s = as
        | r >  s = dropInner (r - s) as
        | otherwise = (a, s - r) : as
    dropInner _ [] = []

take :: Ratio Natural -> ListFraction a -> ListFraction a
take r0 (ListFraction f0) = ListFraction (takeInner r0 f0)
  where
    takeInner r ((a, s) : as)
        | r == 0 = []
        | r == s = [(a, s)]
        | r >  s = (a, s) : takeInner (r - s) as
        | otherwise = [(a, r)]
    takeInner _ [] = []

splitAt :: Ratio Natural -> ListFraction a -> (ListFraction a, ListFraction a)
splitAt r f = (take r f, drop r f)

isPrefixOf :: Eq a => ListFraction a -> ListFraction a -> Bool
isPrefixOf a b = a == take (length a) b

isSuffixOf :: Eq a => ListFraction a -> ListFraction a -> Bool
isSuffixOf a b = a == drop (getSum (Sum (length b) <\> Sum (length a))) b

isInfixOf :: Eq a => ListFraction a -> ListFraction a -> Bool
isInfixOf (ListFraction f1) (ListFraction f2) = test f1 f2
  where
    test [] _ = True
    test _ [] = False
    test ((a, p) : as) ((b, q) : bs) =
        (a == b && p <= q && ListFraction as `isPrefixOf` ListFraction bs)
        ||
        test ((a, p) : as) bs

isValid :: Eq a => ListFraction a -> Bool
isValid f = f == toCanonical f
