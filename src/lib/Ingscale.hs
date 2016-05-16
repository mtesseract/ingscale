-- IngScale - Library for convenient scaling of ingredients lists.
-- Copyright (C) 2015-2016 Moritz Schulte <mtesseract@silverratio.net>

-- API is not necessarily stable.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Ingscale (Ingredient(..),
                 conversionFactor,
                 convertQuantity,
                 equivalentQuantities,
                 lookupBaseUnit,
                 lookupUnitsByBase,
                 parseNumber,
                 parseQuantity,
                 parseIngredient,
                 parseIngredients,
                 roundQuantity,
                 computeScalingFactorIng,
                 approximateQuantity,
                 scaleIngredients,
                 printQuantity,
                 printIngredient,
                 printIngredients,
                 printIngredientsExt) where

import           Control.Lens
import           Data.Function (on)
import           Data.List
import           Data.Maybe -- (listToMaybe)
import qualified Data.Ratio as R
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Formatting
import           Quantities
import           Quantities.Types
import           Quantities.Printer

quotient :: Integral a => a -> a -> R.Ratio a
quotient a b = a R.% b

---------------
-- Datatypes --
---------------

-- | An Ingredient is the combination of an ingredient name and a
-- Quantity.
data Ingredient = Ingredient { ingredientName     :: Text
                             , ingredientQuantity :: Quantity } deriving (Show)

makeFields ''Ingredient

-----------------------------------------
-- Ingscale Specific Utility Functions --
-----------------------------------------

-- | Given an ingredients list INGREDIENTS and a NAME, try to extract
-- the single ingredient by that name. Returns Nothing if INGREDIENTS
-- does not contain an ingredient by that name.
extractIngredient :: [Ingredient] -> Text -> Maybe Ingredient
extractIngredient ingredients n =
  listToMaybe $ filter ((==) n . view name) ingredients

-- | Like extractIngredient, but do not compute the complete
-- Ingredient datatype, return only its contained Quantity.
extractIngredientQuantity :: [Ingredient] -> Text -> Maybe Quantity
extractIngredientQuantity ingredients n =
 view quantity <$> extractIngredient ingredients n

-------------
-- Scaling --
-------------

-- | Scale the ingredients list INGREDIENTS by the factor FACTOR.
scaleIngredients :: Rational -> [Ingredient] -> [Ingredient]
scaleIngredients factor =
  traverse . quantity %~ (roundQuantity . scaleQuantity factor)
  where scaleQuantity = (number *~)

-- | Given a list of INGREDIENTS and another INGREDIENT, compute the
-- factor by which the ingredients list has to be scaled such that the
-- ingredient with the same name as INGREDIENT contained in
-- INGREDIENTS has exactly the quantity of INGREDIENT.
computeScalingFactorIng :: [Ingredient] -> Ingredient -> Either Text Rational
computeScalingFactorIng ingredients i = do
  q1 <- maybeToEither "Ingredient not found in list" $
    extractIngredientQuantity ingredients (i ^. name)
  let q2 = i ^. quantity
  computeScalingFactor q1 q2

---------------------
-- Pretty Printers --
---------------------

-- | Pretty print a single Ingredient.
printIngredient :: Ingredient -> Text
printIngredient i =
  format (text % ", " % text)
         (i ^. name)
         (printQuantity printNumber (i ^. quantity))

-- | Pretty print an ingredients list.
printIngredients :: [Ingredient] -> Text
printIngredients ingredients = T.unlines $ map printIngredient ingredients

type IngredientExt = Ingredient -> Text

printIngredientExt :: IngredientExt -> Ingredient -> Text
printIngredientExt iExt i =
  format (text % text) (printIngredient i) (iExt i)

printIngredientsExt :: [Ingredient] -> IngredientExt -> Text
printIngredientsExt ingredients iExt =
  T.unlines $ map (printIngredientExt iExt) ingredients


--------------
-- Rounding --
--------------

-- | This is the list of denomitors we prefer, when possible. e.g.,
-- during printing Rationals or when trying to clever approximate
-- quantities.
goodDenominators :: [Integer]
goodDenominators = [2, 3, 4]

-- | List of "good" fractions, derived from goodDenominators above.
goodFractions :: [Rational]
goodFractions = [0, 1] ++ map (uncurry quotient) goodPairs
  where goodPairs = concatMap (\ d -> zip [1..d-1] (repeat d)) goodDenominators

-- | Try to approximate a number in a clever way. I.e., use fractions
-- from goodFractions if possible.
approximateNumber :: Rational -> Rational -> Rational
approximateNumber epsilon x =
    let xInt       = fromIntegral (floor x :: Integer)
        fraction   = x - xInt
        allowedErr = epsilon * x -- Heuristic describing what the
                                 -- neglectable error is. Make it
                                 -- depend on the actual value to be
                                 -- approximated.
    in xInt + tryApproxRational allowedErr fraction

  where tryApproxRational allowedErr y =
          let approximations         = map (\ frac -> (frac, abs (y - frac))) goodFractions
              approximationsFiltered = filter (\ (_, err) -> err < allowedErr) approximations
              approximationsSorted   = sortBy (compare `on` snd) approximationsFiltered
          in fromMaybe y (fst <$> listToMaybe approximationsSorted)

unitDigits :: [(Unit, Int)]
unitDigits =
  [ (UnitL,    3)
  , (UnitML,   0)
  , (UnitCup,  2)
  , (UnitTSP,  2)
  , (UnitTBSP, 2)
  , (UnitFLOZ, 2)
  , (UnitKG,   3)
  , (UnitG,    0)
  , (UnitOZ,   2)
  , (UnitLB,   2) ]
    

-- | Try to round a given quantity, using the unitSpecRound field
-- contained in the UnitSpecification. If the quantity's denominator
-- is contained in goodDeminator, return the number unmodified.  If
-- for some reason the UnitSpecification cannot be found, return the
-- number unmodified.
roundQuantity :: QuantityTransformer
roundQuantity q =
  if R.denominator (q^.number) `elem` goodDenominators
     then q
     else let nDigits   = fromMaybe defaultNDigits (lookup (q ^. unit) unitDigits)
          in q & number %~ roundIt nDigits

  where roundIt nDigits num =
          fromIntegral (round (num * 10^^nDigits) :: Integer) / 10^^nDigits

        defaultNDigits :: Int
        defaultNDigits = 2

-- | Quantity Transformer: Approximate in a clever way.
approximateQuantity :: Rational -> QuantityTransformer
approximateQuantity epsilon = number %~ approximateNumber epsilon


---------------
-- Utilities --
---------------

-- | Utility function for converting Maybe values to Either values.
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing  = Left  a

showText :: Show a => a -> T.Text
showText = T.pack . show

-------------
-- Parsers --
-------------
-- | Parse a single Ingredient.
parseIngredient :: Text -> Either Text Ingredient
parseIngredient ingredientString = do
  (ingName, ingQuant) <- splitIngredient ingredientString
  q <- parseQuantity parseNumber ingQuant
  return $ Ingredient ingName q
  where splitIngredient ingS =
          let stringWords = T.splitOn "," ingS
          in case stringWords of
               [a,b] -> Right (a,b)
               _     -> Left $ errMsg ingS

        -- errMsg :: Text -> Text
        errMsg = format ("Malformed ingredient line '" % text %  "'")

-- | Parse input as an ingredients list. Each line has to be of the
-- form:
--
--   <Ingredient Name>, <Number> <Unit Abbreviation>\n
--
parseIngredients :: Text -> Either Text [Ingredient]
parseIngredients ingredientsString =
    foldl parseAndAdd (Right []) (T.lines ingredientsString)
  where parseAndAdd ingredientsList' ingredientString = do
          ingredientsList <- ingredientsList'
          ingredient <- parseIngredient ingredientString
          Right $ ingredientsList ++ [ingredient]

