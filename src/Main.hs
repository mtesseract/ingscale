-- IngScale - Program for convenient scaling of ingredients lists.
-- Copyright (C) 2015-2016 Moritz Schulte <mtesseract@silverratio.net>

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Monad
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import           Data.Typeable
import           Ingscale
import           Options.Applicative

data IngscaleException = ExceptionString Text | ExceptionNone
    deriving (Show, Typeable)

instance Exception IngscaleException

showText :: Show a => a -> Text
showText = T.pack . show

programName :: Text
programName = "ingscale"

programVersion :: Text
programVersion = "0.1-git"

programDescription :: Text
programDescription = "Program for convenient scaling of ingredients lists"

ingscalePrintIngredients :: Rational -> [Ingredient] -> Text
ingscalePrintIngredients epsilon ingredients =
  printIngredientsExt ingredients iExt
  where iExt :: Ingredient -> Text
        iExt i =
          let q = ingredientQuantity i
              qs = equivalentQuantities q
              qs' = map (approximateQuantity epsilon . roundQuantity) qs
          in if null qs'
                then ""
                else T.concat [" [", T.intercalate ", " (map printQuantity qs'), "]"]
              
ingscaleByFactor :: Rational -> Rational -> IO ()
ingscaleByFactor epsilon factor = do
  input <- TIO.getContents
  let ingredients = ingscaleError' (parseIngredients input)
      ingredients' = scaleIngredients factor ingredients
  TIO.putStr $ ingscalePrintIngredients epsilon ingredients'

ingscaleToQuantity :: Rational -> Ingredient -> IO ()
ingscaleToQuantity epsilon ingredient = do
  input <- TIO.getContents
  let ingredients = ingscaleError' (parseIngredients input)
      factor = ingscaleError' (computeScalingFactor ingredients ingredient)
      ingredients' = scaleIngredients factor ingredients
  TIO.putStr $ ingscalePrintIngredients epsilon ingredients'

ingscale :: IngscaleOptions -> IO ()
ingscale opts = do
  let epsilon = (ingscaleError' . parseNumber . T.pack . optsAllowedErr) opts
  when (optsVersion opts) $ do
    TIO.putStrLn $ T.concat [programName, " ", programVersion]
    throw ExceptionNone
  case (null (optsScaleTo opts), null (optsScaleBy opts)) of
    (True,  True)  -> ingscaleError "Either --scale-to or --scale-by must be given"
    (False, False) -> ingscaleError "--scale-to and --scale-by given at the same time"
    (True,  False) -> do let scaleBy = T.pack $ optsScaleBy opts
                             factor  = either ingscaleError id $ parseNumber scaleBy
                         ingscaleByFactor epsilon factor
    (False, True)  -> do let scaleTo    = T.pack $ optsScaleTo opts
                             ingredient = either ingscaleError id (parseIngredient scaleTo)
                         ingscaleToQuantity epsilon ingredient

-- Throw an IngscaleException.
ingscaleError :: Text -> a
ingscaleError msg = throw (ExceptionString msg)

-- Throw an IngscaleException on Left, return Right otherwise.
ingscaleError' :: Either Text a -> a
ingscaleError' = either ingscaleError id

main' :: IngscaleOptions -> IO ()
main' opts =
  catch (ingscale opts)
        (\ e -> case (e :: IngscaleException) of
                  ExceptionString s -> TIO.putStrLn $ T.concat ["Error: ", s]
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
                  <> header (T.unpack programName ++ " - " ++ T.unpack programDescription))
