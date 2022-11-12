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
    { inputs
        :: [Coin a]
    , outputs
        :: [Coin a]
    , change
        :: [Coin a]
    }

renderSelectionAsGrid
    :: forall a. (Ord a, Show a) => Selection a -> Widget ()
renderSelectionAsGrid selection =
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
        [ show . getAssetValue a <$> outputs selection
        , show . getAssetValue a <$> inputs  selection
        ]

    rcolumn :: [String]
    rcolumn = "" : mconcat
        [ "Change" <$ change selection
        ]

    rcolumnsForAssets :: [[String]]
    rcolumnsForAssets = rcolumnForAsset <$> toList (selectionAssets selection)

    rcolumnForAsset :: a -> [String]
    rcolumnForAsset a = show a : mconcat
        [ show . getAssetValue a <$> change selection
        ]

selectionAssets :: Ord a => Selection a -> Set a
selectionAssets Selection {inputs, outputs, change} = mconcat
    [ foldMap getAssets inputs
    , foldMap getAssets outputs
    , foldMap getAssets change
    ]

coinMaxCoinValue :: Ord a => Coin a -> CoinValue
coinMaxCoinValue c
    | toList c == mempty = mempty
    | otherwise = maximum (snd <$> toList c)

selectionMaxCoinValue :: Ord a => Selection a -> CoinValue
selectionMaxCoinValue s = maximum $ coinMaxCoinValue <$> mconcat
    [ toList (inputs  s)
    , toList (outputs s)
    , toList (change  s)
    ]

renderSelection :: forall a. (Ord a, Show a) => Selection a -> Widget ()
renderSelection s = vBox
    [ renderOutputsInputs
    , renderChange
    ]
  where
    renderOutputsInputs :: Widget ()
    renderOutputsInputs = renderTable $ table $ pure
        [ hBox [str " ", renderCoins (outputs s), str " "]
        , hBox [str " ", renderCoins (inputs  s), str " "]
        ]

    renderChange :: Widget ()
    renderChange = renderTable $ table $ pure
        [ hBox [str " ", renderCoins changeForOutputs, str " "]
        , hBox [str " ", renderCoins changeForInputs , str " "]
        ]
      where
        changeForInputs  = drop (length (outputs s)) (change s)
        changeForOutputs = take (length (outputs s)) (change s)

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
        [ [(A, 30), (B, 30)         ]
        , [         (B, 30), (C, 30)]
        , [(A, 30),          (C, 30)]
        ]
    , outputs =
        [ [(A, 10), (B, 10)         ]
        , [         (B, 10), (C, 10)]
        , [(A, 10),          (C, 10)]
        ]
    , change =
        [ [(A, 20), (B, 20)         ]
        , [         (B, 20), (C, 20)]
        , [(A, 20),          (C, 20)]
        , [(A,  2), (B,  2)         ]
        , [         (B,  2), (C,  2)]
        , [(A,  2),          (C,  2)]
        ]
    }
