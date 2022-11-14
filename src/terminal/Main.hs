{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Change
    ( SelectionOf (..)
    , SelectionWithChange
    , WeightChange (..)
    , exampleSelection
    , makeChange
    , selectionAssets
    )
import Value
    ( CoinValue, getAssetValue )

import qualified Data.Foldable as F

import Brick
import Brick.Widgets.Table

main :: IO ()
main = simpleMain ui

ui :: Widget ()
ui = renderSelectionWithChange (makeChange exampleSelection)

renderSelectionWithChange
    :: forall f a. (Foldable f, Ord a, Show a)
    => SelectionWithChange f a
    -> Widget ()
renderSelectionWithChange selection@Selection{inputs, outputs} =
    renderTable $ tableOptions $ table $ mconcat
        [ [rowAssets]
        , [rowSpacer]
        , rowSelectionChange "Input"
            <$> F.toList inputs
        , [rowSpacer]
        , rowSelectionChange "Output"
            <$> F.toList outputs
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

renderCoinValue :: CoinValue -> Widget ()
renderCoinValue v
    | v == 0 = str mempty
    | otherwise = str (show v)
