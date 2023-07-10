module Internal.Prelude
    ( module Prelude
    , FromFractional (..)
    , FromInteger (..)
    , FromRational (..)
    , IsList (..)
    , Negatable (..)
    , ifThenElse
    , (&)
    )
    where

import Prelude hiding
    ( fromInteger, fromRational, negate )

import Data.Function
    ( (&) )
import Data.Ratio
    ( Ratio )
import Data.Word
    ( Word16, Word32, Word64, Word8 )
import GHC.Exts
    ( IsList (..) )
import Numeric.Natural
    ( Natural )

import qualified Data.Ratio as Ratio
import qualified Prelude

--------------------------------------------------------------------------------
-- FromFractional
--------------------------------------------------------------------------------

class FromFractional a where
    type FractionalPart a
    (%) :: FractionalPart a -> FractionalPart a -> a

instance Integral a => FromFractional (Ratio a) where
    type FractionalPart (Ratio a) = a
    (%) = (Ratio.%)

--------------------------------------------------------------------------------
-- FromInteger
--------------------------------------------------------------------------------

class FromInteger a where
    fromInteger :: Integer -> a

newtype NumFromInteger a = NumFromInteger a

instance Num a => FromInteger (NumFromInteger a) where
    fromInteger a = NumFromInteger $ Prelude.fromInteger a

deriving via NumFromInteger Double
    instance FromInteger Double
deriving via NumFromInteger Int
    instance FromInteger Int
deriving via NumFromInteger Integer
    instance FromInteger Integer
deriving via NumFromInteger Natural
    instance FromInteger Natural
deriving via NumFromInteger Word8
    instance FromInteger Word8
deriving via NumFromInteger Word16
    instance FromInteger Word16
deriving via NumFromInteger Word32
    instance FromInteger Word32
deriving via NumFromInteger Word64
    instance FromInteger Word64
deriving via NumFromInteger (Ratio a)
    instance Integral a => FromInteger (Ratio a)

--------------------------------------------------------------------------------
-- FromRational
--------------------------------------------------------------------------------

class FromRational a where
    fromRational :: Rational -> a

newtype FractionalFromRational a = FractionalFromRational a

instance Fractional a => FromRational (FractionalFromRational a) where
    fromRational a = FractionalFromRational $ Prelude.fromRational a

deriving via FractionalFromRational Double
    instance FromRational Double
deriving via FractionalFromRational Float
    instance FromRational Float
deriving via FractionalFromRational Rational
    instance FromRational Rational

--------------------------------------------------------------------------------
-- Negatable
--------------------------------------------------------------------------------

class Negatable a where
    negate :: a -> a

newtype NumNegatable a = NumNegatable a

instance Num a => Negatable (NumNegatable a) where
    negate (NumNegatable a) = NumNegatable $ Prelude.negate a

deriving via NumNegatable Int
    instance Negatable Int
deriving via NumNegatable Integer
    instance Negatable Integer

--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  a _ = a
ifThenElse False _ a = a
