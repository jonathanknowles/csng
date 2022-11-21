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

class (PartialOrd e, PartialOrd (Exact e)) => ExactBounded e where
    type Exact e
    exact :: e -> Exact e
    lowerBound :: Exact e -> e
    upperBound :: Exact e -> e

exactBoundedLaw_exact_lowerBound
    :: forall e. (ExactBounded e, Eq e) => e -> Bool
exactBoundedLaw_exact_lowerBound b =
    lowerBound @e (exact b) == b

exactBoundedLaw_exact_upperBound
    :: forall e. ( ExactBounded e, Eq e) => e -> Bool
exactBoundedLaw_exact_upperBound b =
    upperBound @e (exact b) == b

exactBoundedLaw_lowerBound_upperBound_equivalence_1
    :: forall e. (ExactBounded e, Eq e) => Exact e -> Bool
exactBoundedLaw_lowerBound_upperBound_equivalence_1 e =
    exact @e (lowerBound e) == e <=> exact @e (upperBound e) == e

exactBoundedLaw_lowerBound_upperBound_equivalence_2
    :: forall e. (ExactBounded e, Eq e) => Exact e -> Bool
exactBoundedLaw_lowerBound_upperBound_equivalence_2 e =
    exact @e (lowerBound e) /= e <=> exact @e (upperBound e) /= e

exactBoundedLaw_lowerBound_leq
    :: forall e. (ExactBounded e, Eq e) => Exact e -> Exact e -> Bool
exactBoundedLaw_lowerBound_leq e1 e2 =
    e1 `leq` e2 <=> lowerBound @e e1 `leq` lowerBound @e e2

exactBoundedLaw_upperBound_leq
    :: forall e. (ExactBounded e, Eq e) => Exact e -> Exact e -> Bool
exactBoundedLaw_upperBound_leq e1 e2 =
    e1 `leq` e2 <=> upperBound @e e1 `leq` upperBound @e e2

exactBoundedLaw_lowerBound_exact_leq
    :: forall e. (ExactBounded e, Eq e) => Exact e -> Bool
exactBoundedLaw_lowerBound_exact_leq e =
    exact @e (lowerBound e) `leq` e

exactBoundedLaw_upperBound_exact_leq
    :: forall e. (ExactBounded e, Eq e) => Exact e -> Bool
exactBoundedLaw_upperBound_exact_leq e =
    e `leq` exact @e (upperBound e)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

instance ExactBounded Natural where
    type Exact Natural = Ratio Natural
    exact = (% 1)
    lowerBound = floor
    upperBound = ceiling

instance ExactBounded (Sum Natural) where
    type Exact (Sum Natural) = Sum (Ratio Natural)
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
