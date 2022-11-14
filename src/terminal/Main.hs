{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Change
    ( SelectionOf (..)
    , SelectionWithChange
    , WeightChange (..)
    , exampleSelection
    , makeChange
    )
import Data.Strict.Set
    ( Set )
import Value
    ( Coin, CoinValue, getAssetValue )

import qualified Data.Foldable as F
import qualified Data.Strict.Set as Set

import Brick
import Brick.Widgets.Table

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
ui = renderSelectionWithChange (makeChange exampleSelection)

main :: IO ()
main = simpleMain ui
