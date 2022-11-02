{-# LANGUAGE AllowAmbiguousTypes #-}
{- HLINT ignore "Use camelCase" -}

module Algebra.ExactBounded where

import Algebra.PartialOrd.Extended
    ( PartialOrd )
import Data.Monoid
    ( Sum )
import Data.Ratio
    ( Ratio )
import Numeric.Natural
    ( Natural )

class (PartialOrd e, PartialOrd b) => ExactBounded e b | b -> e, e -> b where
    toExact :: b -> e
    toLowerBound :: e -> b
    toUpperBound :: e -> b

exactBoundedLaw_toExact_toLowerBound
    :: (ExactBounded e b, Eq b) => b -> Bool
exactBoundedLaw_toExact_toLowerBound b =
    toLowerBound (toExact b) == b

exactBoundedLaw_toExact_toUpperBound
    :: (ExactBounded e b, Eq b) => b -> Bool
exactBoundedLaw_toExact_toUpperBound b =
    toUpperBound (toExact b) == b

exactBoundedLaw_toLowerBound_toUpperBound_equivalence_1
    :: (ExactBounded e b, Eq e) => e -> Bool
exactBoundedLaw_toLowerBound_toUpperBound_equivalence_1 e =
    toExact (toLowerBound e) == e ≡ toExact (toUpperBound e) == e

exactBoundedLaw_toLowerBound_toUpperBound_equivalence_2
    :: (ExactBounded e b, Eq e) => e -> Bool
exactBoundedLaw_toLowerBound_toUpperBound_equivalence_2 e =
    toExact (toLowerBound e) /= e ≡ toExact (toUpperBound e) /= e

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

instance ExactBounded (Ratio Natural) Natural where
    toExact = (% 1)
    toLowerBound = floor
    toUpperBound = ceiling

instance ExactBounded (Sum (Ratio Natural)) (Sum Natural) where
    toExact = fmap (% 1)
    toLowerBound = fmap floor
    toUpperBound = fmap ceiling

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Logical implication.
--
infixr 0 ⇒
(⇒) :: Bool -> Bool -> Bool
a ⇒ b = not a || b

-- | Logical equivalence.
--
infixr 0 ≡
(≡) :: Bool -> Bool -> Bool
a ≡ b = (a ⇒ b) || (b ⇒ a)
