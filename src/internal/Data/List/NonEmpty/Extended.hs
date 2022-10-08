module Data.List.NonEmpty.Extended
    ( module Data.List.NonEmpty
    , splitWhen
    ) where

import Data.List.NonEmpty

import qualified Data.List.NonEmpty as NE

splitWhen :: (a -> a -> Bool) -> NonEmpty a -> (NonEmpty a, [a])
splitWhen f (a :| as) =
    go (a :| []) as
  where
    go (p :| ps) (q : qs) | not (f p q) = go (q <| p :| ps) qs
    go ps qs = (NE.reverse ps, qs)
