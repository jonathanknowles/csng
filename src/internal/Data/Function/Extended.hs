module Data.Function.Extended
    ( module Data.Function
    , applyN
    ) where

import Data.Function

import qualified Data.Foldable as F

-- Apply the same function multiple times to a value.
--
applyN :: Int -> (a -> a) -> a -> a
applyN n f = F.foldr (.) id (replicate n f)
