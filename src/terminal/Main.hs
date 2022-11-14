{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Data.Strict.Set
    ( Set )
import Value
    ( Coin, CoinValue, getAssetValue )

import qualified Data.Foldable as F
import qualified Data.Strict.Set as Set

import Brick
import Brick.Widgets.Table

makeChange :: Selection f a -> SelectionWithChange f a
makeChange = undefined

data SelectionOf f g a = Selection
    { inputs  :: f (g a)
    , outputs :: f (g a)
    }
    deriving Foldable

data WeightChange a = WeightChange
    { weight :: Coin a
    , change :: Coin a
    }
    deriving Foldable

type Selection f a = SelectionOf f Coin a
type SelectionWithChange f a = SelectionOf f WeightChange a

renderSelectionWithChange
    :: forall f a. (Foldable f, Ord a, Show a)
    => SelectionWithChange f a
    -> Widget ()
renderSelectionWithChange selection@Selection{inputs, outputs} =
    renderTable $ tableOptions $ table $ mconcat
        [ [rowAssets]
        , [rowSpacer]
        , rowSelectionChange "Output"
            <$> F.toList outputs
        , [rowSpacer]
        , rowSelectionChange "Input"
            <$> F.toList inputs
        ]
  where
    tableOptions
        = rowBorders False
        . setDefaultColAlignment AlignRight

    assets :: [a]
    assets = toList (selectionAssets selection)

    rowSpacer :: [Widget ()]
    rowSpacer = str " " <$ rowAssets

    rowAssets :: [Widget ()]
    rowAssets = mconcat
        [ [str "Asset"]
        , str . show <$> assets
        , [str " "]
        , [str "Asset"]
        , str . show <$> assets
        ]

    rowSelectionChange :: String -> WeightChange a -> [Widget ()]
    rowSelectionChange entryType e = mconcat
        [ [str entryType]
        , renderCoinValue . flip getAssetValue (weight e) <$> assets
        , [str " "]
        , [str "Change"]
        , renderCoinValue . flip getAssetValue (change e) <$> assets
        ]

selectionAssets :: (Foldable f, Foldable g, Ord a) => SelectionOf f g a -> Set a
selectionAssets = F.foldMap Set.singleton

coinMaxCoinValue :: Ord a => Coin a -> CoinValue
coinMaxCoinValue c
    | toList c == mempty = mempty
    | otherwise = maximum (snd <$> toList c)

selectionMaxCoinValue :: Ord a => SelectionWithChange [] a -> CoinValue
selectionMaxCoinValue Selection {inputs, outputs} =
    maximum $ coinMaxCoinValue <$> mconcat
        [ toList (weight <$> (inputs <> outputs))
        , toList (change <$> (inputs <> outputs))
        ]

renderCoinValue :: CoinValue -> Widget ()
renderCoinValue v
    | v == 0 = str mempty
    | otherwise = str (show v)

ui :: Widget ()
ui = renderSelectionWithChange exampleSelection

main :: IO ()
main = simpleMain ui

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

data ExampleAsset = A | B | C | D
    deriving (Eq, Ord, Show)

exampleCoin :: Coin ExampleAsset
exampleCoin = [(A, 1000000), (B, 20), (C, 3000)]

exampleSelection :: SelectionWithChange [] ExampleAsset
exampleSelection = Selection
    { inputs =
        [ WeightChange [(A, 30), (B, 30)         ] [(A, 20), (B, 20)         ]
        , WeightChange [         (B, 30), (C, 30)] [         (B, 20), (C, 20)]
        , WeightChange [(A, 30),          (C, 30)] [(A, 20),          (C, 20)]
        ]
    , outputs =
        [ WeightChange [(A, 10), (B, 10)         ] [(A,  2), (B,  2)         ]
        , WeightChange [         (B, 10), (C, 10)] [         (B,  2), (C,  2)]
        , WeightChange [(A, 10),          (C, 10)] [(A,  2),          (C,  2)]
        ]
    }
