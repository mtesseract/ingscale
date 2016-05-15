-- IngScale - Library for convenient scaling of ingredients lists.
-- Copyright (C) 2015-2016 Moritz Schulte <mtesseract@silverratio.net>

-- API is not necessarily stable.

{-# LANGUAGE FlexibleContexts       #-}

module Ingscale.Parser
       ( parseNumber
       , parseQuantity
       , parseIngredient
       , parseIngredientText
       , parseIngredients
       , parseIngredientsText
       ) where

import           Control.Lens
import           Data.Char
import           Data.List.Split
import           Data.Maybe
import           Data.String.Utils
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Ingscale.Types
import           Ingscale.Units
import           Numeric (readFloat, readSigned)
import           Text.Read

-------------
-- Parsers --
-------------

-- | Convert a String into a Unit.
stringToUnit :: String -> Unit
stringToUnit "" = UnitNone -- Degenerate case of a unitless Quantity,
                           -- i.e. only a number.
stringToUnit string = fromMaybe (UnitOther string) (lookupUnitName string)

-- | Extract the unit names, forming (String Name, Unit) pairs.
lookupUnitName :: String -> Maybe Unit
lookupUnitName n = listToMaybe $ _units^..ifolded.withIndex.filtered matchName._1
  where matchName (_, s) = n `elem` (s ^. name : s ^. aliases)
  
-- | Try to parse an Integer.
parseInt :: String -> Either String Integer
parseInt s =
  let result = readMaybe s :: Maybe Integer
  in case result of
       Nothing -> Left $ "Failed to parse integer `" ++ s ++ "'"
       Just x  -> Right x

-- | Try to parse a fraction of the form "x/y" into a Rational.
parseFraction :: String -> Either String Rational
parseFraction s =
  let result = readMaybe (replace "/" "%" s)
  in case result of
       Nothing -> Left $ "Failed to parse fraction `" ++ s ++ "'"
       Just x  -> Right x

-- | Try to parse a decimal number.
parseDecimalNumber :: String -> Either String Rational
parseDecimalNumber s =
    let result = readSigned readFloat s :: [(Rational, String)]
    in case result of
         [(x, "")] -> Right x
         _        -> Left $ "Failed to parse decimal number `" ++ s ++ "'"

-- | Try to parse a mixed number, e.g. a number of the form "5 23/42".
parseMixed :: String -> Either String Rational
parseMixed s =
  let components = splitWs s
  in case components of
    [c0] -> if '/' `elem` c0
            then parseFraction c0
            else fromInteger <$> parseInt c0
    [c0, c1] -> do int <- parseInt c0
                   frac <- parseFraction c1
                   case (int < 0, frac < 0) of
                     (False, False) -> Right $ fromInteger int + frac
                     (True, False)  -> Right $ (-1) * (fromInteger (abs int) + frac)
                     (False, True)  -> Left errNoParse
                     (True, True)   -> Left errNoParse
    _ -> Left errNoParse
  where errNoParse = "No Parse"

-- | Try to parse a given number in string representation. First try
-- to parse it as a decimal number and if that fails, try to parse it
-- as a mixed number.
parseNumber :: String -> Either String Rational
parseNumber s' =
  let s = strip s'
  in eitherAlternative "parseNumber: Internal Error"
       [parseDecimalNumber s, parseMixed s]

-- | Return first Right value in the list. If the list contains no
-- Right, return the last Left in the list. If the list is empty,
-- return default Left.
eitherAlternative :: a -> [Either a b] -> Either a b
eitherAlternative def []             = Left def
eitherAlternative _   [x]            = x
eitherAlternative _   (Right x : _)  = Right x
eitherAlternative def (Left _  : xs) = eitherAlternative def xs

-- | Parse a given quantity in its string representation, e.g. a
-- string of the form "0.7 l".
parseQuantity :: String -> Either String Quantity
parseQuantity string = do
  let (w0, w1) = splitAtUnit string
      u        = stringToUnit w1
  num <- parseNumber w0
  return $ Quantity num u

  where splitAtUnit s =
          let num = takeWhile (not . isAlpha) s
              u   = drop (length num) s
          in (num, u)

-- | Parse a single Ingredient.
parseIngredient :: String -> Either String Ingredient
parseIngredient ingredientString = do
  (ingName, ingQuant) <- splitIngredient ingredientString
  q <- parseQuantity ingQuant
  return $ Ingredient ingName q
  where splitIngredient ingS =
          let stringWords = linesBy (== ',') ingS
          in case stringWords of
               [a,b] -> Right (a,b)
               _     -> Left $ errMsg ingS

        errMsg :: String -> String
        errMsg s = "parseIngredients: Malformed ingredient line '" ++ s ++ "'"

-- | Just like parseIngredient, except the input data is passed as a
-- Data.Text.Lazy.Text instead of a String.
parseIngredientText :: Text -> Either String Ingredient
parseIngredientText = parseIngredient . T.unpack

-- | Parse input as an ingredients list. Each line has to be of the
-- form:
--
--   <Ingredient Name>, <Number> <Unit Abbreviation>\n
--
parseIngredients :: String -> Either String [Ingredient]
parseIngredients ingredientsString =
    foldl parseAndAdd (Right []) (lines ingredientsString)
  where parseAndAdd ingredientsList' ingredientString = do
          ingredientsList <- ingredientsList'
          ingredient <- parseIngredient ingredientString
          Right $ ingredientsList ++ [ingredient]

-- | Just like parseIngredients, except the input data is passed as a
-- Data.Text.Lazy.Text instead of a String.
parseIngredientsText :: Text -> Either String [Ingredient]
parseIngredientsText = parseIngredients . T.unpack
