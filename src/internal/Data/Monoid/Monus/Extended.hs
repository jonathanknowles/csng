module Data.Monoid.Monus.Extended
    ( module Data.Monoid.Monus
    , distance
    ) where

import Data.Monoid.Monus

-- | Computes the distance between two values.
--
-- = Properties
--
-- == Identity
--    prop> a `distance` a == mempty
--    prop> a `distance` mempty == a
--    prop> mempty `distance` a == a
--
-- == Commutativity
--    prop> a `distance` b == b `distance` a
--
-- == Associativity
--    prop> (a `distance` b) `distance` c == a `distance` (b `distance` c)
--
-- == Transitivity
--    prop> (a `distance` b) `distance` (b `distance` c) == a `distance` c
--
-- = Examples
--
-- >>> getSum @Natural (2 `distance` 8)
-- 6
-- >>> getSum @Natural (8 `distance` 2)
-- 6
--
-- >>> Set.fromList [1, 2, 3] `distance` Set.fromList [2, 3, 4]
-- fromList [1, 4]
-- >>> Set.fromList [2, 3, 4] `distance` Set.fromList [1, 2, 3]
-- fromList [1, 4]
--
distance :: Monus a => a -> a -> a
distance a b = (a <\> b) <> (b <\> a)
