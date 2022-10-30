module Data.List.NonEmpty.Extended
    ( module Data.List.NonEmpty
    , splitWhen
    , zip3
    ) where

import Data.List.NonEmpty

import Prelude hiding
    ( zip3 )

import qualified Data.List.NonEmpty as NE

splitWhen :: (a -> a -> Bool) -> NonEmpty a -> (NonEmpty a, [a])
splitWhen f (a :| as) =
    go (a :| []) as
  where
    go (p :| ps) (q : qs) | not (f p q) = go (q <| p :| ps) qs
    go ps qs = (NE.reverse ps, qs)

zip3 :: NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty (a, b, c)
zip3 as = NE.zipWith (\(a, b) c -> (a, b, c)) . NE.zip as
