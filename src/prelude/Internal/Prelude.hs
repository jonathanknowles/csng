module Internal.Prelude
    ( module Prelude
    , FromInteger (..)
    , FromRational (..)
    , IsList (..)
    , ifThenElse
    , (&)
    )
    where

import Prelude hiding
    ( fromInteger, fromRational )

import Data.Function
    ( (&) )
import Data.Ratio
    ( Ratio )
import GHC.Exts
    ( IsList (..) )
import Numeric.Natural
    ( Natural )

import qualified Prelude

class FromInteger a where
    fromInteger :: Integer -> a

instance FromInteger Double where
    fromInteger = Prelude.fromInteger

instance FromInteger Int where
    fromInteger = Prelude.fromInteger

instance FromInteger Natural where
    fromInteger = fromIntegral

instance FromInteger (Ratio Natural) where
    fromInteger = fromIntegral

class FromRational a where
    fromRational :: Rational -> a

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  a _ = a
ifThenElse False _ a = a
