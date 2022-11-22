{-# LANGUAGE AllowAmbiguousTypes #-}
{- HLINT ignore "Use camelCase" -}

module Algebra.ExactBounded where

import Algebra.PartialOrd.Extended
    ( PartialOrd (..) )
import Data.Monoid
    ( Sum )
import Data.Ratio
    ( Ratio )
import Numeric.Natural
    ( Natural )

class (PartialOrd e, PartialOrd b) => BoundedExact b e | b -> e, e -> b
  where
    exact :: b -> e
    lowerBound :: e -> b
    upperBound :: e -> b

boundedExactLaw_exact_lowerBound
    :: (BoundedExact b e, Eq b) => b -> Bool
boundedExactLaw_exact_lowerBound b =
    lowerBound (exact b) == b

boundedExactLaw_exact_upperBound
    :: (BoundedExact b e, Eq b) => b -> Bool
boundedExactLaw_exact_upperBound b =
    upperBound (exact b) == b

boundedExactLaw_lowerBound_upperBound_equivalence_1
    :: (BoundedExact b e, Eq e) => e -> Bool
boundedExactLaw_lowerBound_upperBound_equivalence_1 e =
    exact (lowerBound e) == e <=> exact (upperBound e) == e

boundedExactLaw_lowerBound_upperBound_equivalence_2
    :: (BoundedExact b e, Eq e) => e -> Bool
boundedExactLaw_lowerBound_upperBound_equivalence_2 e =
    exact (lowerBound e) /= e <=> exact (upperBound e) /= e

boundedExactLaw_lowerBound_leq
    :: (BoundedExact b e, Eq e) => e -> e -> Bool
boundedExactLaw_lowerBound_leq e1 e2 =
    e1 `leq` e2 <=> lowerBound e1 `leq` lowerBound e2

boundedExactLaw_upperBound_leq
    :: (BoundedExact b e, Eq e) => e -> e -> Bool
boundedExactLaw_upperBound_leq e1 e2 =
    e1 `leq` e2 <=> upperBound e1 `leq` upperBound e2

boundedExactLaw_lowerBound_exact_leq
    :: (BoundedExact b e, Eq e) => e -> Bool
boundedExactLaw_lowerBound_exact_leq e =
    exact (lowerBound e) `leq` e

boundedExactLaw_upperBound_exact_leq
    :: (BoundedExact b e, Eq e) => e -> Bool
boundedExactLaw_upperBound_exact_leq e =
    e `leq` exact (upperBound e)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

instance BoundedExact Natural (Ratio Natural) where
    exact = (% 1)
    lowerBound = floor
    upperBound = ceiling

instance BoundedExact (Sum Natural) (Sum (Ratio Natural)) where
    exact = fmap (% 1)
    lowerBound = fmap floor
    upperBound = fmap ceiling

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Logical implication.
--
infixr 0 ==>
(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b

-- | Logical equivalence.
--
infixr 0 <=>
(<=>) :: Bool -> Bool -> Bool
a <=> b = (a ==> b) || (b ==> a)
