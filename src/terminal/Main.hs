{-# LANGUAGE OverloadedLists #-}

module Main where

import Data.List
    ( transpose )
import Data.Strict.Set
    ( Set )
import Value
    ( Coin, CoinValue, getAssets, getAssetValue )

import qualified Data.List as L

import Brick
import Brick.Widgets.Table

data Selection a = Selection
    { inputs  :: [SelectionEntry a]
    , outputs :: [SelectionEntry a]
    }

data SelectionEntry a = SelectionEntry
    { weight :: Coin a
    , change :: Coin a
    }

renderSelection
    :: forall a. (Ord a, Show a) => Selection a -> Widget ()
renderSelection selection@Selection{inputs, outputs} =
    renderTable $ tableOptions $ table $ mconcat
        [ [rowAssets]
        , [rowSpacer]
        , rowSelectionEntry "Output"
            <$> outputs
        , [rowSpacer]
        , rowSelectionEntry "Input"
            <$> inputs
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

    rowSelectionEntry :: String -> SelectionEntry a -> [Widget ()]
    rowSelectionEntry entryType e = mconcat
        [ [str entryType]
        , renderCoinValue . flip getAssetValue (weight e) <$> assets
        , [str " "]
        , [str "Change"]
        , renderCoinValue . flip getAssetValue (change e) <$> assets
        ]

renderSelection1
    :: forall a. (Ord a, Show a) => Selection a -> Widget ()
renderSelection1 selection =
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
        [ "Output" <$ outputs selection
        , "Input"  <$ inputs  selection
        ]

    lcolumnsForAssets :: [[String]]
    lcolumnsForAssets = lcolumnForAsset <$> toList (selectionAssets selection)

    lcolumnForAsset :: a -> [String]
    lcolumnForAsset a = show a : mconcat
        [ show . getAssetValue a . weight <$> outputs selection
        , show . getAssetValue a . weight <$> inputs  selection
        ]

    rcolumn :: [String]
    rcolumn = "" : mconcat
        [ "Change" <$ outputs selection
        , "Change" <$ inputs  selection
        ]

    rcolumnsForAssets :: [[String]]
    rcolumnsForAssets = rcolumnForAsset <$> toList (selectionAssets selection)

    rcolumnForAsset :: a -> [String]
    rcolumnForAsset a = show a : mconcat
        [ show . getAssetValue a . change <$> outputs selection
        , show . getAssetValue a . change <$> inputs  selection
        ]

selectionAssets :: Ord a => Selection a -> Set a
selectionAssets Selection {inputs, outputs} = mconcat
    [ foldMap (getAssets . weight) (inputs <> outputs)
    , foldMap (getAssets . change) (inputs <> outputs)
    ]

coinMaxCoinValue :: Ord a => Coin a -> CoinValue
coinMaxCoinValue c
    | toList c == mempty = mempty
    | otherwise = maximum (snd <$> toList c)

selectionMaxCoinValue :: Ord a => Selection a -> CoinValue
selectionMaxCoinValue Selection {inputs, outputs} =
    maximum $ coinMaxCoinValue <$> mconcat
        [ toList (weight <$> (inputs <> outputs))
        , toList (change <$> (inputs <> outputs))
        ]

renderSelection2 :: forall a. (Ord a, Show a) => Selection a -> Widget ()
renderSelection2 s = vBox
    [ renderOutputsInputs
    , renderChange
    ]
  where
    renderOutputsInputs :: Widget ()
    renderOutputsInputs = renderTable $ table $ pure
        [ hBox [str " ", renderCoins (weight <$> outputs s), str " "]
        , hBox [str " ", renderCoins (weight <$> inputs  s), str " "]
        ]

    renderChange :: Widget ()
    renderChange = renderTable $ table $ pure
        [ hBox [str " ", renderCoins (change <$> outputs s), str " "]
        , hBox [str " ", renderCoins (change <$> inputs  s), str " "]
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
ui = renderSelection exampleSelection

main :: IO ()
main = simpleMain ui

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------

data ExampleAsset = A | B | C | D
    deriving (Eq, Ord, Show)

exampleCoin :: Coin ExampleAsset
exampleCoin = [(A, 1000000), (B, 20), (C, 3000)]

exampleSelection :: Selection ExampleAsset
exampleSelection = Selection
    { inputs =
        [ SelectionEntry [(A, 30), (B, 30)         ] [(A, 20), (B, 20)         ]
        , SelectionEntry [         (B, 30), (C, 30)] [         (B, 20), (C, 20)]
        , SelectionEntry [(A, 30),          (C, 30)] [(A, 20),          (C, 20)]
        ]
    , outputs =
        [ SelectionEntry [(A, 10), (B, 10)         ] [(A,  2), (B,  2)         ]
        , SelectionEntry [         (B, 10), (C, 10)] [         (B,  2), (C,  2)]
        , SelectionEntry [(A, 10),          (C, 10)] [(A,  2),          (C,  2)]
        ]
    }
