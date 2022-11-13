{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Data.List
    ( transpose )
import Data.Strict.Set
    ( Set )
import GHC.Records
    ( HasField (..) )
import Value
    ( Coin, CoinValue, getAssetValue )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Strict.Set as Set

import Brick
import Brick.Widgets.Table

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

renderSelectionWithChange1
    :: forall a. (Ord a, Show a) => SelectionWithChange [] a -> Widget ()
renderSelectionWithChange1 selection =
    renderTable $ setDefaultColAlignment AlignRight $ table $ transpose columns
  where
    columns :: [[Widget ()]]
    columns = fmap (str . (<> " ") . (" " <>)) <$> mconcat
        [ [lcolumn]
        , lcolumnsForAssets
        , rcolumnsForAssets
        , [rcolumn]
        ]

    lcolumn :: [String]
    lcolumn = "" : mconcat
        [ "Output" <$ selection.outputs
        , "Input"  <$ selection.inputs
        ]

    lcolumnsForAssets :: [[String]]
    lcolumnsForAssets = lcolumnForAsset <$> toList (selectionAssets selection)

    lcolumnForAsset :: a -> [String]
    lcolumnForAsset a = show a : mconcat
        [ show . getAssetValue a . weight <$> selection.outputs
        , show . getAssetValue a . weight <$> selection.inputs
        ]

    rcolumn :: [String]
    rcolumn = "" : mconcat
        [ "Change" <$ selection.outputs
        , "Change" <$ selection.inputs
        ]

    rcolumnsForAssets :: [[String]]
    rcolumnsForAssets = rcolumnForAsset <$> toList (selectionAssets selection)

    rcolumnForAsset :: a -> [String]
    rcolumnForAsset a = show a : mconcat
        [ show . getAssetValue a . change <$> selection.outputs
        , show . getAssetValue a . change <$> selection.inputs
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

renderSelectionWithChange2
    :: forall a. (Ord a, Show a) => SelectionWithChange [] a -> Widget ()
renderSelectionWithChange2 s = vBox
    [ renderOutputsInputs
    , renderChange
    ]
  where
    renderOutputsInputs :: Widget ()
    renderOutputsInputs = renderTable $ table $ pure
        [ hBox [str " ", renderCoins (weight <$> s.outputs), str " "]
        , hBox [str " ", renderCoins (weight <$> s.inputs ), str " "]
        ]

    renderChange :: Widget ()
    renderChange = renderTable $ table $ pure
        [ hBox [str " ", renderCoins (change <$> s.outputs), str " "]
        , hBox [str " ", renderCoins (change <$> s.inputs ), str " "]
        ]

    renderCoins :: [Coin a] -> Widget ()
    renderCoins coins = hBox $ L.intersperse (str " ") $
        renderCoin (selectionAssets s) (selectionMaxCoinValue s) <$> coins

renderCoin
    :: forall a. (Ord a, Show a)
    => Set a
    -> CoinValue
    -> Coin a
    -> Widget ()
renderCoin as maxCoinValue c
    = renderTable
    $ surroundingBorder True
    $ setColAlignment AlignLeft  0
    $ setColAlignment AlignRight 1
    $ columnBorders False
    $ rowBorders False
    $ table
    $ renderAssetValue <$> assetValues
  where
    assetValues :: [(a, CoinValue)]
    assetValues = (\a -> (a, getAssetValue a c)) <$> toList as

    renderAssetValue :: (a, CoinValue) -> [Widget ()]
    renderAssetValue (a, v)
        | v == 0    = [str " "     , str " ", renderValue v]
        | otherwise = [str (show a), str ":", renderValue v]

    renderValue :: CoinValue -> Widget ()
    renderValue v =
        str (replicate (maxCoinValueWidth - length vString) ' ' <> vString)
      where
        vString
            | v == 0 = ""
            | otherwise = show v

    maxCoinValueWidth = length (show maxCoinValue)

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
