module Data.List.Fraction
    ( ListFraction
    , fromList
    , roundDown
    , roundUp
    )
    where

import Data.Function
    ( on )
import Data.List
    ( groupBy )
import Data.Monoid
    ( Sum (..) )
import Data.Ratio
    ( Ratio )
import Numeric.Natural
    ( Natural )
import Roundable
    ( Roundable (..) )

import Prelude hiding
    ( fromList )

newtype ListFraction a = ListFraction [(a, Ratio Natural)]
    deriving (Eq, Show)

instance Eq a => Semigroup (ListFraction a) where
    ListFraction xs <> ListFraction ys = coalesce (ListFraction (xs <> ys))

instance Eq a => Monoid (ListFraction a) where
    mempty = ListFraction mempty

instance Roundable (ListFraction a) where
    type Rounded (ListFraction a) = [a]
    roundUp (ListFraction as) = do
        (a, n) <- fmap roundUp <$> as
        replicate (fromIntegral n) a
    roundDown (ListFraction as) = do
        (a, n) <- fmap roundDown <$> as
        replicate (fromIntegral n) a

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
