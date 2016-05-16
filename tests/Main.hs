{-# LANGUAGE OverloadedStrings #-}

module Main where

--import           Data.List
--import           Data.Monoid (mempty)
import           Data.Ratio
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T (unpack, concat)
import           Ingscale
--import           Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import           Test.Framework (Test, defaultMain, testGroup)
--import           Test.Framework.Options (TestOptions, TestOptions'(..))
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2 (testProperty)
--import           Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import           Test.HUnit
--import           Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: [Test.Framework.Test]
tests = [ testGroup "Quantity Conversion"
          [ 
            testProperty "Base Unit Conversion" prop_base_conversion
          , testProperty "General Quantity Conversion" prop_conversion
          , testCase "Number Parsing" test_number_parsing
          ]
        , testGroup "Parsing"
          [ 
            testCase "Number Parsing" test_number_parsing
          ]
        , testGroup "Printing"
          [
          ]
        ]

units :: [Unit]
units = [UnitL, UnitML, UnitCup, UnitTSP, UnitTBSP, UnitFLOZ, UnitKG, UnitG, UnitOZ, UnitLB]

extractEither :: Text -> Either Text b -> b
extractEither msg (Left  a) = error (T.unpack (T.concat [msg, ": ", a]))
extractEither _   (Right b) = b


prop_base_conversion :: Rational -> Bool
prop_base_conversion x =
  null $ filter not $ map prop_base_conversion_unit units
  where prop_base_conversion_unit u =
          let q   = Quantity x u
              bU  = extractEither "base unit not found" $
                lookupBaseUnit u
              q'  = extractEither "failed to convert to base unit" $
                convertQuantity q bU
              q'' = extractEither "failed to convert from base unit" $
                convertQuantity q' u
          in q == q''

prop_conversion :: Rational -> Bool
prop_conversion x =
  null $ filter not $ concatMap prop_conversion_unit units
  where prop_conversion_unit u =
          let q   = Quantity x u
              units' = lookupUnitsByBase u
          in map (convert_quantity q) units'

convert_quantity :: Quantity -> Unit -> Bool
convert_quantity q u =
  let q'  = extractEither "failed to convert Quantity" $
        convertQuantity q u
      q'' = extractEither "failed to convert Quantity" $
        convertQuantity q' (quantityUnit q)
  in q == q''
        
number_parsing_table :: [(Text, Maybe Rational)]
number_parsing_table =
  [ ("0", Just 0)
  , ("-0", Just 0)
  , ("0/1", Just 0)
  , ("1/1", Just 1)
  , ("0.5", Just (1 % 2))
  ]

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right b) = Just b

test_number_parsing :: Assertion
test_number_parsing =
  mapM_ (\ (s, res) -> res @?= eitherToMaybe (parseNumber s))
    number_parsing_table
