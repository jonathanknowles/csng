{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Sliceable.Fractional where

import Algebra.ExactBounded
    ( BoundedExact (..) )
import Algebra.PartialOrd.Extended
    ( Infix (..), PartialOrd (..) )
import Data.Monoid
    ( Sum (..) )
import Data.Monoid.Monus.Extended
    ( (<\>) )
import Data.Ratio
    ( Ratio )
import Data.Sliceable
    ( Sliceable )
import Numeric.Natural
    ( Natural )

import Prelude hiding
    ( drop, splitAt, take )

import qualified Data.Sliceable as Sliceable

newtype FractionalSlice f a = FractionalSlice (f (a, Ratio Natural))

deriving instance Eq (f (a, Ratio Natural)) => Eq (FractionalSlice f a)

instance Eq (f (a, Ratio Natural)) => PartialOrd (Infix (FractionalSlice f a))
  where
    leq = undefined

instance
    ( Eq (f (a, Ratio Natural))
    , PartialOrd (Infix (f a))
    ) =>
    BoundedExact (Infix (f a)) (Infix (FractionalSlice f a))
  where
    exact = undefined
    lowerBound = undefined
    upperBound = undefined

instance Foldable f => Sliceable (FractionalSlice f a) where
    type SliceableSize (FractionalSlice f a) = Ratio Natural
    drop = drop
    take = take
    splitAt = splitAt
    size = size

size :: Foldable f => FractionalSlice f a -> Ratio Natural
size (FractionalSlice as) = getSum $ foldMap (Sum . snd) as

isPrefixOf
    :: Foldable f
    => Eq (f (a, Ratio Natural))
    => FractionalSlice f a
    -> FractionalSlice f a
    -> Bool
isPrefixOf a b = a == take (size a) b

isSuffixOf
    :: Foldable f
    => Eq (f (a, Ratio Natural))
    => FractionalSlice f a
    -> FractionalSlice f a
    -> Bool
isSuffixOf a b = a == drop (getSum (Sum (size b) <\> Sum (size a))) b

drop :: Ratio Natural -> FractionalSlice f a -> FractionalSlice f a
drop = undefined

take :: Ratio Natural -> FractionalSlice f a -> FractionalSlice f a
take = undefined

splitAt
    :: Ratio Natural
    -> FractionalSlice f a
    -> (FractionalSlice f a, FractionalSlice f a)
splitAt r f = (take r f, drop r f)
