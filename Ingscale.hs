-- IngScale - Library for convenient scaling of ingredients lists.
-- Copyright (C) 2015-2016 Moritz Schulte <mtesseract@silverratio.net>

-- API is not necessarily stable.

{-# LANGUAGE CPP #-}

module Ingscale (Unit(..),
                 Quantity(..),
                 Ingredient(..),
                 conversionFactor,
                 convertQuantity,
                 equivalentQuantities,
                 parseNumber,
                 parseQuantity,
                 parseIngredient,
                 parseIngredientText,
                 parseIngredients,
                 parseIngredientsText,
                 roundQuantity,
                 computeScalingFactor,
                 computeScalingFactorQuantity,
                 approximateQuantity,
                 transformIngredient,
                 transformIngredients,
                 scaleIngredients,
                 lookupBaseUnit,
                 lookupUnitsByBase,
                 printQuantity,
                 printIngredient,
                 printIngredients,
                 printIngredientsExt) where

import           Control.Monad (when)
import           Data.Char
import           Data.Either
import           Data.Function (on)
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Ratio
import           Data.String.Utils
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Formatting (format, float)
import           Numeric (readFloat, readSigned)
import           Text.Read

---------------
-- Datatypes --
---------------

-- | These are the supported units. UnitNone is for the degenerate
-- case of missing unit.
data Unit = UnitNone
          | UnitL
          | UnitML
          | UnitCup
          | UnitTSP
          | UnitTBSP
          | UnitFLOZ
          | UnitG
          | UnitKG
          | UnitLB
          | UnitOZ
          | UnitOther String deriving (Eq, Show, Read)

-- | A UnitSpec datatype defines a unit completely.
data UnitSpec = UnitSpec
  { unitspecName       :: String   -- Abbreviation of this unit.
  , unitspecBase       :: Unit     -- The unit in which terms this unit is defined.
  , unitspecConversion :: Rational -- Conversion factor from this unit to the base unit.
  , unitspecRound      :: Int      -- Number of digits after a decimal point we want.
  , unitspecAliases    :: [String] -- A list of abbreviation aliases for this unit.
  } deriving (Show)

-- | A Quantity is the combination of a (rational) number and a Unit.
data Quantity = Quantity { quantityNumber :: Rational
                         , quantityUnit :: Unit
                         } deriving (Show, Eq)

-- | An Ingredient is the combination of an ingredient name and a
-- Quantity.
data Ingredient = Ingredient { ingredientName     :: String
                             , ingredientQuantity :: Quantity } deriving (Show)

-------------------------
-- Unit Specifications --
-------------------------

-- | This list contains all unit specifications.
unitSpecifications :: [(Unit, UnitSpec)]
unitSpecifications =
  [(UnitL, -- Liter
    UnitSpec { unitspecName       = "l"
             , unitspecBase       = UnitL
             , unitspecConversion = 1
             , unitspecRound      = 3
             , unitspecAliases    = [] }),
   (UnitML, -- Mililiter
    UnitSpec { unitspecName       = "ml"
             , unitspecBase       = UnitL
             , unitspecConversion = 0.001
             , unitspecRound      = 0
             , unitspecAliases    = [] }),
   (UnitCup, -- Cup
    UnitSpec { unitspecName       = "cup"
             , unitspecBase       = UnitL
             , unitspecConversion =  0.236588236
             , unitspecRound      = 2
             , unitspecAliases    = ["cups"] }),
   (UnitTSP, -- Teaspoon
    UnitSpec { unitspecName       = "tsp"
             , unitspecBase       = UnitL
             , unitspecConversion = 0.00492892159
             , unitspecRound      = 2
             , unitspecAliases    = [] }),
   (UnitTBSP, -- Tablespoon
    UnitSpec { unitspecName       = "tbsp"
             , unitspecBase       = UnitL
             , unitspecConversion = 0.0147867648
             , unitspecRound      = 2
             , unitspecAliases    = [] }),
   (UnitFLOZ, -- Fluid Ounce
    UnitSpec { unitspecName       = "fl.oz"
             , unitspecBase       = UnitL
             , unitspecConversion = 0.0295735295625
             , unitspecRound      = 2
             , unitspecAliases    = [] }),
   (UnitKG, -- Kilogram
    UnitSpec { unitspecName       = "kg"
             , unitspecBase       = UnitKG
             , unitspecConversion = 1
             , unitspecRound      = 3
             , unitspecAliases    = [] }),
   (UnitG, -- Gram
    UnitSpec { unitspecName       = "g"
             , unitspecBase       = UnitKG
             , unitspecConversion = 0.001
             , unitspecRound      = 0
             , unitspecAliases    = [] }),
   (UnitOZ, -- Ounce
    UnitSpec { unitspecName       = "oz"
             , unitspecBase       = UnitKG
             , unitspecConversion = 0.0283495
             , unitspecRound      = 2
             , unitspecAliases    = [] }),
   (UnitLB, -- Pound
    UnitSpec { unitspecName       = "lb"
             , unitspecBase       = UnitKG
             , unitspecConversion = 0.45359237
             , unitspecRound      = 2
             , unitspecAliases    = [] })]

-----------------------------------------
-- Ingscale Specific Utility Functions --
-----------------------------------------

-- | Extract the unit names, forming (String Name, Unit) pairs.
unitNames :: [(String, Unit)]
unitNames =
  concatMap (\ (unit, spec) -> zip (extractNames spec) (repeat unit))
    unitSpecifications
  where extractNames spec = unitspecName spec : unitspecAliases spec

-- | Convert a String into a Unit.
stringToUnit :: String -> Unit
stringToUnit "" = UnitNone -- Degenerate case of a unitless Quantity,
                           -- i.e. only a number.
stringToUnit string =
  fromMaybe (UnitOther string) (lookup string unitNames)

-- | Extract Units for a specified base Unit.
filterUnitsByBase :: Unit -> [Unit]
filterUnitsByBase unit =
    map fst $ filter (matchBaseUnit unit) unitSpecifications
  where matchBaseUnit u (_, spec) = u == unitspecBase spec

-- | Given a Unit, lookup its UnitSpecification. Note that UnitOther
-- has no Unit specification, hence we need a Maybe type here.
lookupUnitSpec :: Unit -> Maybe UnitSpec
lookupUnitSpec unit = lookup unit unitSpecifications

-- | General function for looking up a UnitSpecification and computing
-- a value from that Specification.
lookupUnitSpec' :: Unit -> (UnitSpec -> a) -> Maybe a
lookupUnitSpec' unit transformer = transformer <$> lookupUnitSpec unit

-- | Lookup the conversion factor for a given unit.
lookupConversionFactor :: Unit -> Either String Rational
lookupConversionFactor unit =
  maybeToEither errMsg (lookupUnitSpec' unit unitspecConversion)
  where errMsg = "failed to lookup conversion factor for unit " ++ show unit

lookupUnitsByBase :: Unit -> [Unit]
lookupUnitsByBase b = catMaybes $
  map (\ (u, s) -> if unitspecBase s == b
                   then Just u
                   else Nothing)
    unitSpecifications
  

-- | Given a Unit, lookup its base Unit (every Unit needs a base
-- Unit!).
lookupBaseUnit :: Unit -> Either String Unit
lookupBaseUnit unit =
  maybeToEither errMsg (lookupUnitSpec' unit unitspecBase)
  where errMsg = "Failed to lookup base unit for unit " ++ printUnit unit

-- | Given an ingredients list INGREDIENTS and a NAME, try to extract
-- the single ingredient by that name. Returns Nothing if INGREDIENTS
-- does not contain an ingredient by that name.
extractIngredient :: [Ingredient] -> String -> Maybe Ingredient
extractIngredient ingredients name =
    let result = filter (\ ingredient -> ingredientName ingredient == name) ingredients
    in if null result
       then Nothing
       else Just (head result)

-- | Like extractIngredient, but do not compute the complete
-- Ingredient datatype, return only its contained Quantity.
extractIngredientQuantity :: [Ingredient] -> String -> Maybe Quantity
extractIngredientQuantity ingredients name =
 ingredientQuantity <$> extractIngredient ingredients name

-------------------------------
-- General Utility Functions --
-------------------------------

-- | Utility function for converting Maybe values to Either values.
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing  = Left  a

---------------------
-- Pretty Printers --
---------------------

-- | Pretty printing for Units.
printUnit :: Unit -> String
printUnit unit =
    case unit of
      UnitNone -> ""
      UnitOther s -> s
      _ -> let spec = lookup unit unitSpecifications
           in maybe "" unitspecName spec

-- | Convert a rational number into its string representation as a
-- mixed number; e.g. 1 % 2 => "1/2".
printMixed :: Rational -> String
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
                     else show int
        fraction = if rest == 0
                      then ""
                      else show rest ++ "/" ++ show denom
    in -- Assemble the final string:
       concat ["",
               -- Handle minus sign:
               if minus then "-" else "",
               -- Then the integer prefix:
               prefix,
               -- If prefix and fraction is both non-empty, then we
               -- need to add a blank:
               if null prefix || null fraction then "" else " ",
               -- And finally the fraction:
               fraction]

-- | Convert a rational number into a convenient string
-- representation: If the denominator is contained in a list of "good"
-- denominators then display the number as a mixed number, otherwise
-- display it as a real number.
printRational :: Rational -> String
printRational x =
    let denom = abs (denominator x)
    in if denom `elem` goodDenominators
       then printMixed x
       else T.unpack $ format float x

-- | Pretty print a Quantity.
printQuantity :: Quantity -> String
printQuantity Quantity { quantityNumber = number, quantityUnit = unit } =
  printRational number ++ " " ++ printUnit unit

-- | Pretty print a single Ingredient.
printIngredient :: Ingredient -> String
printIngredient i =
  let name     = ingredientName i
      quantity = ingredientQuantity i
  in name ++ ", " ++ printQuantity quantity

-- | Pretty print an ingredients list.
printIngredients :: [Ingredient] -> String
printIngredients ingredients = unlines $ map printIngredient ingredients

type IngredientExt = Ingredient -> String

printIngredientExt :: IngredientExt -> Ingredient -> String
printIngredientExt iExt i =
  printIngredient i ++ iExt i

printIngredientsExt :: [Ingredient] -> IngredientExt -> String
printIngredientsExt ingredients iExt =
  unlines $ map (printIngredientExt iExt) ingredients

-- | Given a Quantity, Compute the list of "equivalent Quantities".
equivalentQuantities :: Quantity -> [Quantity]
equivalentQuantities q =
  let unit = quantityUnit q
      baseUnit = lookupBaseUnit unit
  in case baseUnit of
       Left _      -> []
       Right bUnit -> let units = filter (/= unit) (filterUnitsByBase bUnit)
                      in rights $ map (convertQuantity q) units

-------------
-- Parsers --
-------------

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
      unit     = stringToUnit w1
  num <- parseNumber w0
  return Quantity { quantityNumber = num, quantityUnit = unit }

  where splitAtUnit s =
          let num  = takeWhile (not . isAlpha) s
              unit = drop (length num) s
          in (num, unit)

-- | Parse a single Ingredient.
parseIngredient :: String -> Either String Ingredient
parseIngredient ingredientString = do
  (ingName, ingQuant) <- splitIngredient ingredientString
  quantity <- parseQuantity ingQuant
  return Ingredient { ingredientName = ingName
                    , ingredientQuantity = quantity }
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

-------------
-- Scaling --
-------------

-- | Quantity Transformer: Scale a given Quantity by a given factor.
scaleQuantity :: Rational -> QuantityTransformer
scaleQuantity factor (Quantity number unit) =
  Quantity { quantityNumber = number * factor
           , quantityUnit   = unit }

-- | Scale the ingredient INGREDIENT by the factor FACTOR.
scaleIngredient :: Rational -> Ingredient -> Ingredient
scaleIngredient factor ingredient =
  let name = ingredientName ingredient
      quantity = ingredientQuantity ingredient
      quantity' = scaleQuantity factor quantity
  in Ingredient { ingredientName = name, ingredientQuantity = quantity' }

-- | Scale the ingredients list INGREDIENTS by the factor FACTOR.
scaleIngredients :: Rational -> [Ingredient] -> [Ingredient]
scaleIngredients factor ingredients = 
  let ingredients' = map (scaleIngredient factor) ingredients
  in transformIngredients roundQuantity ingredients'

-- | Given a list of INGREDIENTS and another INGREDIENT, compute the
-- factor by which the ingredients list has to be scaled such that the
-- ingredient with the same name as INGREDIENT contained in
-- INGREDIENTS has exactly the quantity of INGREDIENT.
computeScalingFactor :: [Ingredient] -> Ingredient -> Either String Rational
computeScalingFactor ingredients ingredient = do
  let name = ingredientName ingredient
  quantity1 <- maybeToEither "Ingredient not found in list" $
                 extractIngredientQuantity ingredients name
  let quantity2 = ingredientQuantity ingredient
  computeScalingFactorQuantity quantity1 quantity2

-- quantity1 * computeScalingFactor' = quantity2
computeScalingFactorQuantity :: Quantity -> Quantity -> Either String Rational
computeScalingFactorQuantity (Quantity num1 unit1) (Quantity num2 unit2) = do
  when (num1 == 0) $ Left "Quantity is zero."
  conversionUnit <- conversionFactor unit1 unit2
  return $ conversionUnit * (num2 / num1)

---------------------
-- Unit Conversion --
---------------------

-- | Compute conversion factor required for transforming between
-- Units: fromUnit * conversionFactor(fromUnit, toUnit) = toUnit
conversionFactor :: Unit -> Unit -> Either String Rational
conversionFactor fromUnit toUnit
  | fromUnit == toUnit = Right 1 -- Short cut. Also necessary for
                                 -- UnitOther values, which do not
                                 -- have a common base unit.
  | otherwise = do
      fromBaseUnit <- lookupBaseUnit fromUnit
      toBaseUnit   <- lookupBaseUnit toUnit
      if fromBaseUnit == toBaseUnit
         then do factor1 <- lookupConversionFactor toUnit
                 factor2 <- lookupConversionFactor fromUnit
                 return $ factor1 / factor2
         else Left $ "Base unit mismatch for units "
                       ++ printUnit fromUnit ++ " and " ++ printUnit toUnit

-- | If possible, convert a given Quantity into a new Quantity using
-- the specified Unit. Returns Nothing if the Unit of the specified
-- Quantity cannot be converted to the specified Unit.
convertQuantity :: Quantity -> Unit -> Either String Quantity
convertQuantity (Quantity number unit) toUnit = do
  factor <- conversionFactor unit toUnit
  return Quantity { quantityNumber = number / factor
                  , quantityUnit   = toUnit }

-------------------------------------------------------------------
-- Quantity & Ingredient Transformers (Rounding & Approximation) --
-------------------------------------------------------------------

type QuantityTransformer = Quantity -> Quantity

-- | This is the list of denomitors we prefer, when possible. e.g.,
-- during printing Rationals or when trying to clever approximate
-- quantities.
goodDenominators :: [Integer]
goodDenominators = [2, 3, 4]

-- | List of "good" fractions, derived from goodDenominators above.
goodFractions :: [Rational]
goodFractions = [0, 1] ++ map (uncurry (%)) goodPairs
  where goodPairs :: [(Integer, Integer)]
        goodPairs = concatMap (\ d -> zip [1..d-1] (repeat d)) goodDenominators

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

-- | Try to round a given quantity, using the unitspecRound field
-- contained in the UnitSpecification. If the quantity's denominator
-- is contained in goodDeminator, return the number unmodified.  If
-- for some reason the UnitSpecification cannot be found, return the
-- number unmodified.
roundQuantity :: QuantityTransformer
roundQuantity quantity =
  if quantityHasGoodDenominator quantity
     then quantity
     else let unit      = quantityUnit quantity
              maybeSpec = lookup unit unitSpecifications
              nDigits   = fromMaybe defaultNDigits (unitspecRound <$> maybeSpec)
          in roundIt quantity nDigits

  where quantityHasGoodDenominator Quantity { quantityNumber = x } =
          denominator x `elem` goodDenominators

        roundIt q digits =
          let number  = quantityNumber q
              unit    = quantityUnit q
              number' = fromIntegral (round (number * 10^^digits) :: Integer) / 10^^digits
          in Quantity { quantityNumber = number'
                      , quantityUnit   = unit }

        defaultNDigits :: Int
        defaultNDigits = 2

-- | Quantity Transformer: Approximate in a clever way.
approximateQuantity :: Rational -> QuantityTransformer
approximateQuantity epsilon Quantity { quantityNumber = number
                                     , quantityUnit = unit } =
  let number' = approximateNumber epsilon number
  in Quantity { quantityNumber = number'
              , quantityUnit = unit }

-- | Transform an Ingredient using a Quantity transformer.
transformIngredient :: QuantityTransformer -> Ingredient -> Ingredient
transformIngredient transformer ingredient =
  let quantity = ingredientQuantity ingredient
      quantity' = transformer quantity
  in ingredient { ingredientQuantity = quantity' }

-- | Transform an Ingredients list using a Quantity transformer.
transformIngredients :: QuantityTransformer -> [Ingredient] -> [Ingredient]
transformIngredients transformer =
  map (transformIngredient transformer)
