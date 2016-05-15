-- IngScale - Program for convenient scaling of ingredients lists.
-- Copyright (C) 2015-2016 Moritz Schulte <mtesseract@silverratio.net>

module Main where

import Options.Applicative
import Control.Monad
import Control.Exception
import Data.Typeable
import Data.List

import Ingscale

data IngscaleException = ExceptionString String | ExceptionNone
    deriving (Show, Typeable)

instance Exception IngscaleException

programName :: String
programName = "ingscale"

programVersion :: String
programVersion = "0.1-git"

programDescription :: String
programDescription = "Program for convenient scaling of ingredients lists"

ingscalePrintIngredients :: Rational -> [Ingredient] -> String
ingscalePrintIngredients epsilon ingredients =
  printIngredientsExt ingredients iExt
  where iExt :: Ingredient -> String
        iExt i =
          let quantity = ingredientQuantity i
              quantities = equivalentQuantities quantity
              quantities' = map (approximateQuantity epsilon . roundQuantity) quantities
          in if null quantities'
                then ""
                else " [" ++ intercalate ", " (map printQuantity quantities') ++ "]"
              
ingscaleByFactor :: Rational -> Rational -> IO ()
ingscaleByFactor epsilon factor = do
  input <- getContents
  let ingredients = ingscaleError' (parseIngredients input)
      ingredients' = scaleIngredients factor ingredients
  putStr $ ingscalePrintIngredients epsilon ingredients'

ingscaleToQuantity :: Rational -> Ingredient -> IO ()
ingscaleToQuantity epsilon ingredient = do
  input <- getContents
  let ingredients = ingscaleError' (parseIngredients input)
      factor = ingscaleError' (computeScalingFactor ingredients ingredient)
      ingredients' = scaleIngredients factor ingredients
  putStr $ ingscalePrintIngredients epsilon ingredients'
--  putStrLn $ "scale to " ++ name ++ "|" ++ (printQuantity quantity)

ingscale :: IngscaleOptions -> IO ()
ingscale opts = do
  let epsilon = ingscaleError' (parseNumber (optsAllowedErr opts))
  when (optsVersion opts) $ do
    putStrLn $ programName ++ " " ++ programVersion
    throw ExceptionNone
  case (null (optsScaleTo opts), null (optsScaleBy opts)) of
    (True,  True)  -> ingscaleError "Either --scale-to or --scale-by must be given"
    (False, False) -> ingscaleError "--scale-to and --scale-by given at the same time"
    (True,  False) -> do let factor = either ingscaleError id (parseNumber (optsScaleBy opts))
                         ingscaleByFactor epsilon factor
    (False, True)  -> do let ingredient = either ingscaleError id (parseIngredient (optsScaleTo opts))
                         ingscaleToQuantity epsilon ingredient

-- Throw an IngscaleException.
ingscaleError :: String -> a
ingscaleError msg = throw (ExceptionString msg)

-- Throw an IngscaleException on Left, return Right otherwise.
ingscaleError' :: Either String a -> a
ingscaleError' = either ingscaleError id

main' :: IngscaleOptions -> IO ()
main' opts =
  catch (ingscale opts)
        (\ e -> case (e :: IngscaleException) of
                  ExceptionString s -> putStrLn $ "Error: " ++ s
                  ExceptionNone     -> return ())

data IngscaleOptions = IngscaleOptions
  { optsVersion    :: Bool
  , optsAllowedErr :: String
  , optsScaleTo    :: String
  , optsScaleBy    :: String }

-- Standard error allowed: 5% of each quantity.
allowedError :: Rational
allowedError = 0.05

ingscaleOptions :: Parser IngscaleOptions
ingscaleOptions = IngscaleOptions
  <$> switch (long "version"
              <> help "Display version information")
  <*> strOption (value (show (fromRational allowedError :: Double))
                 <> long "allowed-error"
                 <> metavar "EPSILON"
                 <> help ("Specify the allowed approximation error (in \
                          \percentage, default = " ++ show allowedError ++ ")"))
  <*> strOption (value ""
                 <> long "scale-to"
                 <> metavar "INGREDIENT"
                 <> help "Scale ingredients to a specific ingredient quantity")
  <*> strOption (value ""
                 <> long "scale-by"
                 <> metavar "FACTOR"
                 <> help "Scale ingredients by a factor")

main :: IO ()
main = execParser opts >>= main'
  where opts = info (helper <*> ingscaleOptions)
                 (fullDesc
                  <> header (programName ++ " - " ++ programDescription))
