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

class (PartialOrd e, PartialOrd (Bound e)) => ExactBounded e where
    type Bound e
    exact :: Bound e -> e
    lowerBound :: e -> Bound e
    upperBound :: e -> Bound e

exactBoundedLaw_exact_lowerBound
    :: forall e. (ExactBounded e, Eq (Bound e)) => Bound e -> Bool
exactBoundedLaw_exact_lowerBound b =
    lowerBound @e (exact b) == b

exactBoundedLaw_exact_upperBound
    :: forall e.( ExactBounded e, Eq (Bound e)) => Bound e -> Bool
exactBoundedLaw_exact_upperBound b =
    upperBound @e (exact b) == b

exactBoundedLaw_lowerBound_upperBound_equivalence_1
    :: (ExactBounded e, Eq e) => e -> Bool
exactBoundedLaw_lowerBound_upperBound_equivalence_1 e =
    exact (lowerBound e) == e <=> exact (upperBound e) == e

exactBoundedLaw_lowerBound_upperBound_equivalence_2
    :: (ExactBounded e, Eq e) => e -> Bool
exactBoundedLaw_lowerBound_upperBound_equivalence_2 e =
    exact (lowerBound e) /= e <=> exact (upperBound e) /= e

exactBoundedLaw_lowerBound_leq
    :: (ExactBounded e, Eq e) => e -> e -> Bool
exactBoundedLaw_lowerBound_leq e1 e2 =
    e1 `leq` e2 <=> lowerBound e1 `leq` lowerBound e2

exactBoundedLaw_upperBound_leq
    :: (ExactBounded e, Eq e) => e -> e -> Bool
exactBoundedLaw_upperBound_leq e1 e2 =
    e1 `leq` e2 <=> upperBound e1 `leq` upperBound e2

exactBoundedLaw_lowerBound_exact_leq
    :: (ExactBounded e, Eq e) => e -> Bool
exactBoundedLaw_lowerBound_exact_leq e =
    exact (lowerBound e) `leq` e

exactBoundedLaw_upperBound_exact_leq
    :: (ExactBounded e, Eq e) => e -> Bool
exactBoundedLaw_upperBound_exact_leq e =
    e `leq` exact (upperBound e)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

instance ExactBounded (Ratio Natural) where
    type Bound (Ratio Natural) = Natural
    exact = (% 1)
    lowerBound = floor
    upperBound = ceiling

instance ExactBounded (Sum (Ratio Natural)) where
    type Bound (Sum (Ratio Natural)) = Sum Natural
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
