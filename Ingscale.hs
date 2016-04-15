-- IngScale - Library for convenient scaling of ingredients lists.
-- Copyright (C) 2015 Moritz Schulte <mtesseract@silverratio.net>

-- API is not stable.

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
                 printQuantity,
                 printIngredient,
                 printIngredients,
                 printIngredientsExt) where

import           Control.Monad (when)
import           Data.Char
import           Data.Either
import           Data.List
import           Data.List.Split
import           Data.Ratio
import           Data.String.Utils
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Numeric (readFloat, readSigned)
import           Text.Read

---------------
-- Datatypes --
---------------

-- These are the supported units. UnitNone is for the degenerate case
-- of missing unit.
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

-- A UnitSpec datatype defines a unit completely.
data UnitSpec = UnitSpec
  { unitspecName       :: String   -- Abbreviation of this unit.
  , unitspecBase       :: Unit     -- The unit in which terms this unit is defined.
  , unitspecConversion :: Rational -- Conversion factor from this unit to the base unit.
  , unitspecRound      :: Int      -- Number of digits after a decimal point we want.
  , unitspecAliases    :: [String] -- A list of abbreviation aliases for this unit.
  } deriving (Show)

-- A Quantity is the combination of a (rational) number and a Unit.
data Quantity = Quantity Rational Unit deriving (Show)

-- Accessor function for the number wrapped in a Quantity.
quantityNumber :: Quantity -> Rational
quantityNumber (Quantity number _) = number

-- Accessor function for the unit wrapped in a Quantity.
quantityUnit :: Quantity -> Unit
quantityUnit (Quantity _ unit) = unit

-- An Ingredient is the combination of an ingredient name and a Quantity.
data Ingredient = Ingredient { ingredientName     :: String
                             , ingredientQuantity :: Quantity } deriving (Show)

-------------------------
-- Unit Specifications --
-------------------------

-- This list contains all unit specifications.
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
             , unitspecBase       = UnitG
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
   (UnitLB,
    UnitSpec { unitspecName       = "lb"
             , unitspecBase       = UnitG
             , unitspecConversion = 0.45359237
             , unitspecRound      = 2
             , unitspecAliases    = [] })]

-----------------------------------------
-- Ingscale Specific Utility Functions --
-----------------------------------------

-- Extract the unit names, forming (String Name, Unit) pairs.
unitNames :: [(String, Unit)]
unitNames = concat $
    map (\ (unit, spec) -> zip (extractNames spec) (repeat unit)) unitSpecifications
  where extractNames spec = [unitspecName spec] ++ (unitspecAliases spec)

-- Convert a String into a Unit.
stringToUnit :: String -> Unit
stringToUnit "" = UnitNone -- Degenerate case of a unitless Quantity,
                           -- i.e. only a number.
stringToUnit string =
  maybe (UnitOther string) id (lookup string unitNames)

-- Extract Units for a specified base Unit.
filterUnitsByBase :: Unit -> [Unit]
filterUnitsByBase unit =
    map fst $ filter (matchBaseUnit unit) unitSpecifications
  where matchBaseUnit u (_, spec) = u == unitspecBase spec

-- Given a Unit, lookup its UnitSpecification. Note that UnitOther has
-- no Unit specification, hence we need a Maybe type here.
lookupUnitSpec :: Unit -> Maybe UnitSpec
lookupUnitSpec unit = lookup unit unitSpecifications

-- General function for looking up a UnitSpecification and computing a
-- value from that Specification.
lookupUnitSpec' :: Unit -> (UnitSpec -> a) -> Maybe a
lookupUnitSpec' unit transformer = maybe Nothing (Just . transformer) (lookupUnitSpec unit)

-- Lookup the conversion factor for a given unit.
lookupConversionFactor :: Unit -> Maybe Rational
lookupConversionFactor unit = lookupUnitSpec' unit unitspecConversion

-- Given a Unit, lookup its base Unit (every Unit needs a base Unit!).
lookupBaseUnit :: Unit -> Maybe Unit
lookupBaseUnit unit = lookupUnitSpec' unit unitspecBase

-- Given an ingredients list INGREDIENTS and a NAME, try to extract
-- the single ingredient by that name. Returns Nothing if INGREDIENTS
-- does not contain an ingredient by that name.
extractIngredient :: [Ingredient] -> String -> Maybe Ingredient
extractIngredient ingredients name =
    let result = filter (\ ingredient -> ingredientName ingredient == name) ingredients
    in if null result
       then Nothing
       else Just (head result)

-- Like extractIngredient, but do not compute the complete Ingredient
-- datatype, return only its contained Quantity.
extractIngredientQuantity :: [Ingredient] -> String -> Maybe Quantity
extractIngredientQuantity ingredients name =
 fmap ingredientQuantity (extractIngredient ingredients name)

-------------------------------
-- General Utility Functions --
-------------------------------

---------------------
-- Pretty Printers --
---------------------

-- Pretty printing for Units.
printUnit :: Unit -> String
printUnit unit =
    case unit of
      UnitNone -> ""
      UnitOther s -> s
      _ -> let spec = lookup unit unitSpecifications
           in maybe "" unitspecName spec

-- Convert a rational number into its string representation as a mixed
-- number; e.g. 1 % 2 => "1/2".
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
                      else (show rest) ++ "/" ++ (show denom)
    in -- Assemble the final string:
       concat $ ["",
                 -- Handle minus sign:
                 if minus then "-" else "",
                 -- Then the integer prefix:
                 prefix,
                 -- If prefix and fraction is both non-empty, then we
                 -- need to add a blank:
                 if (null prefix || null fraction) then "" else " ",
                 -- And finally the fraction:
                 fraction]

-- Convert a rational number into a convenient string representation:
-- If the denominator is contained in a list of "good" denominators
-- then display the number as a mixed number, otherwise display it as
-- a real number.
printRational :: Rational -> String
printRational x' =
    let x = abs x'
        denom = denominator x
    in if denom `elem` goodDenominators
       then printMixed x'
       else let xReal = fromRational x' :: Double
                xInt = round xReal
            in if xReal - (fromInteger xInt) == 0
               then show xInt
               else show xReal

-- Pretty print a Quantity.
printQuantity :: Quantity -> String
printQuantity (Quantity number unit) = (printRational number) ++ " " ++ (printUnit unit)

-- Pretty print a single Ingredient.
printIngredient :: Ingredient -> String
printIngredient i =
  let name     = ingredientName i
      quantity = (ingredientQuantity i)
  in name ++ ", " ++ (printQuantity quantity)

-- Pretty print an ingredients list.
printIngredients :: [Ingredient] -> String
printIngredients ingredients = unlines $ map printIngredient ingredients

type IngredientExt = Ingredient -> String

printIngredientExt :: IngredientExt -> Ingredient -> String
printIngredientExt iExt i =
  (printIngredient i) ++ (iExt i)

printIngredientsExt :: [Ingredient] -> IngredientExt -> String
printIngredientsExt ingredients iExt =
  unlines $ map (printIngredientExt iExt) ingredients

-- Given a Quantity, Compute the list of "equivalent Quantities".
equivalentQuantities :: Quantity -> [Quantity]
equivalentQuantities q =
  let unit = quantityUnit q
      baseUnit = lookupBaseUnit unit
  in case baseUnit of
       Nothing    -> []
       Just bUnit -> let units = filter (/= unit) (filterUnitsByBase bUnit)
                     in rights $ map (convertQuantity q) units

-------------
-- Parsers --
-------------

-- Try to parse an Integer.
parseInt :: String -> Either String Integer
parseInt s =
  let result = readMaybe s :: Maybe Integer
  in case result of
       Nothing -> Left $ "Failed to parse integer `" ++ s ++ "'"
       Just x  -> Right x

-- Try to parse a fraction of the form "x/y" into a Rational.
parseFraction :: String -> Either String Rational
parseFraction s =
  let result = readMaybe (replace "/" "%" s)
  in case result of
       Nothing -> Left $ "Failed to parse fraction `" ++ s ++ "'"
       Just x  -> Right x

-- Try to parse a decimal number.
parseDecimalNumber :: String -> Either String Rational
parseDecimalNumber s =
    let result = readSigned readFloat s :: [(Rational, String)]
    in case result of
         [(x, "")] -> Right x
         _        -> Left $ "Failed to parse decimal number `" ++ s ++ "'"

-- Try to parse a mixed number, e.g. a number of the form "5 23/42".
parseMixed :: String -> Either String Rational
parseMixed s =
    let components = splitWs s
    in case components of
         [c0] -> if elem '/' c0
                    then parseFraction c0
                    else do int <- parseInt c0
                            return $ fromInteger int
         [c0, c1] -> do int <- parseInt c0
                        let intAbs = abs int
                        frac <- parseFraction c1
                        case (int < 0, frac < 0) of
                          (False, False) -> Right $ (fromInteger int) + frac
                          (True, False)  -> Right $ (-1) * ((fromInteger intAbs) + frac)
                          (False, True)  -> Left "fixme"
                          (True, True)   -> Left "fixme"
         _ -> Left "fixme"

-- Try to parse a given number in string representation. First try to
-- parse it as a decimal number and if that fails, try to parse it as
-- a mixed number.
parseNumber :: String -> Either String Rational
parseNumber s =
    let s' = strip s
        double = parseDecimalNumber s'
    in either (\ _ -> parseMixed s') Right double

-- Parse a given quantity in its string representation, e.g. a string
-- of the form "Water, 0.7 l".
parseQuantity :: String -> Either String Quantity
parseQuantity string =
    let sWords = splitAtUnit string
    in case sWords of
        [w]      -> do number <- parseNumber w
                       return $ Quantity number UnitNone
        [w0, w1] -> do number <- parseNumber w0
                       let unit = stringToUnit w1
                       return $ Quantity number unit
        _ -> error $ "parseQuantity: splitAtUnit returned list " ++ (show sWords)

  where splitAtUnit s = splitAtFirstLetter s ""
        splitAtFirstLetter s sNonChars =
          case s of
            [] -> [sNonChars]
            (s0:rest) -> if isAlpha s0
                            then [sNonChars, s]
                            else splitAtFirstLetter rest (sNonChars ++ [s0])

parseIngredient :: String -> Either String Ingredient
parseIngredient ingredientString = do
  (ingName, ingQuant) <- splitIngredient ingredientString
  ingQuant' <- parseQuantity ingQuant
  return $ Ingredient { ingredientName = ingName, ingredientQuantity = ingQuant' }
  where splitIngredient :: String -> Either String (String, String)
        splitIngredient string =
          let stringWords = linesBy (==',') string
          in case stringWords of
               [a,b] -> Right (a,b)
               _     -> Left $ "parseIngredients: Malformed ingredient line '" ++ string ++ "'"

parseIngredientText :: Text -> Either String Ingredient
parseIngredientText = parseIngredient . T.unpack

-- Parse input as an ingredients list. Each line has to be of the form:
--
--   <Ingredient Name>, <Number> <Unit Abbreviation>\n
--
parseIngredients :: String -> Either String [Ingredient]
parseIngredients ingredientsString =
    foldl parseAndAdd (Right []) (lines ingredientsString)
  where parseAndAdd :: (Either String [Ingredient]) -> String -> (Either String [Ingredient])
        parseAndAdd ingredientsList' ingredientString = do
          ingredientsList <- ingredientsList'
          ingredient <- parseIngredient ingredientString
          Right $ ingredientsList ++ [ingredient]

-- Just like parseIngredients, except the input data is passed as a
-- Data.Text.Lazy.Text instead of a String.
parseIngredientsText :: Text -> Either String [Ingredient]
parseIngredientsText = parseIngredients . T.unpack

-------------
-- Scaling --
-------------

-- Quantity Transformer: Scale a given Quantity by a given factor.
scaleQuantity :: Rational -> QuantityTransformer
scaleQuantity factor (Quantity number unit) = Quantity (number * factor) unit

-- Scale the ingredient INGREDIENT by the factor FACTOR.
scaleIngredient :: Rational -> Ingredient -> Ingredient
scaleIngredient factor ingredient =
  let name = ingredientName ingredient
      quantity = ingredientQuantity ingredient
      quantity' = scaleQuantity factor quantity
  in Ingredient { ingredientName = name, ingredientQuantity = quantity' }

-- Scale the ingredients list INGREDIENTS by the factor FACTOR.
scaleIngredients :: Rational -> [Ingredient] -> [Ingredient]
scaleIngredients factor ingredients = 
  let ingredients' = map (scaleIngredient factor) ingredients
  in transformIngredients roundQuantity ingredients'

-- Given a list of INGREDIENTS and another INGREDIENT, compute the
-- factor by which the ingredients list has to be scaled such that the
-- ingredient with the same name as INGREDIENT contained in
-- INGREDIENTS has exactly the quantity of INGREDIENT.
computeScalingFactor :: [Ingredient] -> Ingredient -> Either String Rational
computeScalingFactor ingredients ingredient = do
    quantity1 <- maybe (Left "Ingredient not found in list") Right $
                  extractIngredientQuantity ingredients (ingredientName ingredient)
    quantity2 <- Right $ ingredientQuantity ingredient
    computeScalingFactorQuantity quantity1 quantity2

-- quantity1 * computeScalingFactor' = quantity2
computeScalingFactorQuantity :: Quantity -> Quantity -> Either String Rational
computeScalingFactorQuantity (Quantity num1 unit1) (Quantity num2 unit2) = do
    when (num1 == 0) $ Left "Quantity is zero."
    conversionUnit <- conversionFactor unit1 unit2
    conversionNum <- Right (num2 / num1)
    return $ conversionUnit * conversionNum

---------------------
-- Unit Conversion --
---------------------

-- fromUnit * conversionFactor = toUnit
conversionFactor :: Unit -> Unit -> Either String Rational
conversionFactor fromUnit toUnit
  | fromUnit == toUnit = Right 1
  | otherwise = do
      let maybeFromBaseUnit = lookupBaseUnit fromUnit
          maybeToBaseUnit = lookupBaseUnit toUnit
      if (maybeFromBaseUnit == maybeToBaseUnit
          && maybeFromBaseUnit /= Nothing
          && maybeToBaseUnit /= Nothing)
         then do factor1 <- maybe (failOnConversion toUnit)   Right $ lookupConversionFactor toUnit
                 factor2 <- maybe (failOnConversion fromUnit) Right $ lookupConversionFactor fromUnit
                 Right $ factor1 / factor2
         else Left $ "Failed to lookup Base Units for units "
                       ++ (printUnit fromUnit) ++ " and " ++ (printUnit toUnit) ++ ""
  where failOnConversion :: Unit -> Either String Rational
        failOnConversion unit = Left $
          "failed to lookup conversion factor for unit " ++ (show unit)

-- If possible, convert a given Quantity into a new Quantity using the
-- specified Unit. Returns Nothing if the Unit of the specified
-- Quantity cannot be converted to the specified Unit.
convertQuantity :: Quantity -> Unit -> Either String Quantity
convertQuantity (Quantity number unit) toUnit = do
  factor <- conversionFactor unit toUnit
  return $ Quantity (number / factor) toUnit

-------------------------------------------------------------------
-- Quantity & Ingredient Transformers (Rounding & Approximation) --
-------------------------------------------------------------------

type QuantityTransformer = Quantity -> Quantity

-- This is the list of denomitors we prefer, when possible. e.g.,
-- during printing Rationals or when trying to clever approximate
-- quantities.
goodDenominators :: [Integer]
goodDenominators = [2, 3, 4]

-- List of "good" fractions, derived from goodDenominators above.
goodFractions :: [Rational]
goodFractions = [0, 1] ++ (map (\ (n, d) -> n % d) goodPairs)
  where goodPairs :: [(Integer, Integer)]
        goodPairs = concat $ map (\ d -> zip [1..d-1] (repeat d)) goodDenominators

-- Try to approximate a number in a clever way. I.e., use fractions
-- from goodFractions if possible.
approximateNumber :: Rational -> Rational -> Rational
approximateNumber epsilon x =
    let xInt = fromIntegral (floor x :: Integer)
        rest = x - xInt
        allowedErr = epsilon * x
        rest' = tryApproxRational allowedErr rest
    in xInt + rest'
  where tryApproxRational :: Rational -> Rational -> Rational
        tryApproxRational allowedErr x' =
          let approximations = map (\ frac -> (frac, abs (x' - frac))) goodFractions
              approximationsFiltered = filter (\ (_, err) -> err < allowedErr) approximations
              approximationsSorted = sortBy (\ pair1 pair2 -> compare (snd pair1) (snd pair2)) approximationsFiltered
          in if null approximationsSorted
                then x'
                else (fst . head) approximationsSorted

-- Try to round a given quantity, using the unitspecRound field
-- contained in the UnitSpecification. If the quantity's denominator
-- is contained in goodDeminator, return the number unmodified.  If
-- for some reason the UnitSpecification cannot be found, return the
-- number unmodified.
roundQuantity :: QuantityTransformer
roundQuantity quantity =
  if quantityHasGoodDenominator quantity
     then quantity
     else let unit = quantityUnit quantity
          in maybe quantity (roundIt quantity) (lookup unit unitSpecifications)

  where quantityHasGoodDenominator :: Quantity -> Bool
        quantityHasGoodDenominator (Quantity x _) =
          (denominator x) `elem` goodDenominators

        roundIt :: Quantity -> UnitSpec -> Quantity
        roundIt q spec =
          let digits  = unitspecRound spec
              number  = quantityNumber q
              unit    = quantityUnit q
              number' = (fromIntegral (round (number * 10^^digits) :: Integer)) / 10^^digits
          in Quantity number' unit

-- Quantity Transformer: Approximate in a clever way.
approximateQuantity :: Rational -> QuantityTransformer
approximateQuantity epsilon (Quantity number unit) =
  let number' = approximateNumber epsilon number
  in Quantity number' unit

-- Transform an Ingredient using a Quantity transformer.
transformIngredient :: QuantityTransformer -> Ingredient -> Ingredient
transformIngredient transformer ingredient =
  let quantity = ingredientQuantity ingredient
      quantity' = transformer quantity
  in ingredient { ingredientQuantity = quantity' }

-- Transform an Ingredients list using a Quantity transformer.
transformIngredients :: QuantityTransformer -> [Ingredient] -> [Ingredient]
transformIngredients transformer ingredients =
  map (transformIngredient transformer) ingredients
