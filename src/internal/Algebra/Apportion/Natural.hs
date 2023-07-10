-- |
--
-- Apportionment of natural numbers.
--
module Algebra.Apportion.Natural
    ( apportion
    ) where

import Prelude hiding
    ( round )

import Control.Arrow
    ( (&&&) )
import Data.Function.Extended
    ( applyN )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Ord
    ( Down (..), comparing )
import Numeric.Natural
    ( Natural )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

-- | Apportions a natural number into a number of parts, where the size of each
--   part is proportional to the size of its corresponding element in the given
--   list of weights, and the number of parts is equal to the number of weights.
--
-- Examples:
--
--      >>> apportion 9 (1 :| [1, 1])
--      Just (3 :| [3, 3])
--
--      >>> apportion 10 (1 :| [])
--      10
--
--      >>> apportion 30 (1 :| [2, 4, 8])
--      Just (2 :| [4, 8, 16])
--
-- Pre-condition: there must be at least one non-zero weight.
--
-- If the pre-condition is not satisfied, this function returns 'Nothing'.
--
-- If the pre-condition is satisfied, this function guarantees that:
--
--  1.  The length of the resulting list is identical to the length of the
--      specified list:
--
--      >>> fmap length (apportion n weights) == Just (length weights)
--
--  2.  The sum of elements in the resulting list is equal to the original
--      natural number:
--
--      >>> fmap sum (apportion n weights) == Just n
--
--  3.  The size of each element in the resulting list is within unity of the
--      ideal proportion.
--
apportion
    :: Natural
        -- ^ Natural number to apportion
    -> NonEmpty Natural
        -- ^ List of weights
    -> Maybe (NonEmpty Natural)
apportion target weights
    | totalWeight == 0 = Nothing
    | otherwise = Just portionsRounded
  where
    portionsRounded :: NonEmpty Natural
    portionsRounded
        -- 1. Start with the list of unrounded portions:
        = portionsUnrounded
        -- 2. Attach an index to each portion, so that we can remember the
        --    original order:
        & NE.zip indices
        -- 3. Sort the portions so that those that require rounding appear
        --    first, and then in descending order of their sizes, and then
        --    in descending order of index position as a tie-breaker.
        & NE.sortBy (comparing (Down . (((isFractional &&& id) . snd) &&& fst)))
        -- 4. Apply pre-computed roundings to each portion:
        & NE.zipWith (fmap . round) roundings
        -- 5. Restore the original order:
        & NE.sortBy (comparing fst)
        -- 6. Strip away the indices:
        & fmap snd
      where
        indices :: NonEmpty Int
        indices = 0 :| [1 ..]

    portionsUnrounded :: NonEmpty Rational
    portionsUnrounded = computeIdealPortion <$> weights
      where
        computeIdealPortion c
            = fromIntegral target
            * fromIntegral c
            % fromIntegral totalWeight

    roundings :: NonEmpty RoundingDirection
    roundings =
        applyN shortfall (NE.cons RoundUp) (NE.repeat RoundDown)
      where
        shortfall
            = fromIntegral target
            - fromIntegral @Integer
                (F.sum $ round RoundDown <$> portionsUnrounded)

    totalWeight :: Natural
    totalWeight = F.sum weights

isFractional :: Rational -> Bool
isFractional r = (floor r :: Integer) /= (ceiling r :: Integer)

-- | Indicates a rounding direction to be used when converting from a
--   fractional value to an integral value.
--
-- See 'round'.
--
data RoundingDirection
    = RoundUp
      -- ^ Round up to the nearest integral value.
    | RoundDown
      -- ^ Round down to the nearest integral value.
    deriving (Eq, Show)

-- | Use the given rounding direction to round the given fractional value,
--   producing an integral result.
--
round :: (RealFrac a, Integral b) => RoundingDirection -> a -> b
round = \case
    RoundUp -> ceiling
    RoundDown -> floor
