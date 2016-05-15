-- IngScale - Library for convenient scaling of ingredients lists.
-- Copyright (C) 2015-2016 Moritz Schulte <mtesseract@silverratio.net>

-- API is not necessarily stable.

{-# LANGUAGE OverloadedStrings #-}

module Ingscale.Printer
       ( printQuantity
       , printIngredient
       , printIngredients
       , printIngredientsExt) where

import           Control.Lens
import           Data.Ratio
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Formatting (format, float)
import           Ingscale.Quantity
import           Ingscale.Types
import           Ingscale.Units
import           Ingscale.Util

---------------------
-- Pretty Printers --
---------------------

-- | Convert a rational number into its string representation as a
-- mixed number; e.g. 1 % 2 => "1/2".
printMixed :: Rational -> Text
printMixed x' =
    let x      = abs x'          -- Absolute Value of x.
        minus  = x' < 0          -- Sign of x.
        num    = numerator x     -- Numerator of x.
        denom  = denominator x   -- Denominator of x.
        int    = num `div` denom -- Number of times the denominator
                                 -- fits into the numerator...
        rest   = num `mod` denom -- ... and the rest.
        prefix = if int == 0
                     -- If num < denom, then we surely do not have an
                     -- integer prefix.
                     then if rest == 0
                             -- If also the rest is zero, then "0" is
                             -- the final result. Otherwise we simply
                             -- have an empty prefix.
                             then "0"
                             else ""
                     else showText int
        fraction = if rest == 0
                      then ""
                      else T.concat [showText rest, "/", showText denom]
    in -- Assemble the final string:
       T.concat ["",
                 -- Handle minus sign:
                 if minus then "-" else "",
                 -- Then the integer prefix:
                 prefix,
                 -- If prefix and fraction is both non-empty, then we
                 -- need to add a blank:
                 if T.null prefix || T.null fraction then "" else " ",
                 -- And finally the fraction:
                 fraction]

-- | Convert a rational number into a convenient string
-- representation: If the denominator is contained in a list of "good"
-- denominators then display the number as a mixed number, otherwise
-- display it as a real number.
printRational :: Rational -> Text
printRational x =
    let denom = abs (denominator x)
    in if denom `elem` goodDenominators
       then printMixed x
       else format float x

-- | Pretty print a Quantity.
printQuantity :: Quantity -> Text
printQuantity q =
  T.concat [ printRational (q ^. number)
           , " "
           , printUnit (q ^. unit) ]

-- | Pretty print a single Ingredient.
printIngredient :: Ingredient -> Text
printIngredient i =
  T.concat [i ^. name
            , ", "
            , printQuantity (i ^. quantity)]

-- | Pretty print an ingredients list.
printIngredients :: [Ingredient] -> Text
printIngredients ingredients = T.unlines $ map printIngredient ingredients

type IngredientExt = Ingredient -> Text

printIngredientExt :: IngredientExt -> Ingredient -> Text
printIngredientExt iExt i =
  T.concat [printIngredient i, iExt i]

printIngredientsExt :: [Ingredient] -> IngredientExt -> Text
printIngredientsExt ingredients iExt =
  T.unlines $ map (printIngredientExt iExt) ingredients
