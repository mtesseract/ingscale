-- IngScale - Library for convenient scaling of ingredients lists.
-- Copyright (C) 2015-2016 Moritz Schulte <mtesseract@silverratio.net>

-- API is not necessarily stable.

{-# LANGUAGE FlexibleContexts       #-}

module Ingscale.Units
       ( _units
       , printUnit
       , lookupBaseUnit
       , filterUnitsByBase
       , lookupUnitSpec
       , lookupConversionFactor
       , conversionFactor
       ) where

import           Control.Lens
import           Data.Map (Map, fromList)
import qualified Data.Map as M
import           Ingscale.Types
import           Ingscale.Util

-------------------------
-- Unit Specifications --
-------------------------

-- | This list contains all unit specifications.
_units :: Map Unit UnitSpec
_units = fromList
  [(UnitL, -- Liter
    UnitSpec { unitSpecName       = "l"
             , unitSpecBase       = UnitL
             , unitSpecConversion = 1
             , unitSpecDigits     = 3
             , unitSpecAliases    = [] }),
   (UnitML, -- Mililiter
    UnitSpec { unitSpecName       = "ml"
             , unitSpecBase       = UnitL
             , unitSpecConversion = 0.001
             , unitSpecDigits     = 0
             , unitSpecAliases    = [] }),
   (UnitCup, -- Cup
    UnitSpec { unitSpecName       = "cup"
             , unitSpecBase       = UnitL
             , unitSpecConversion =  0.236588236
             , unitSpecDigits     = 2
             , unitSpecAliases    = ["cups"] }),
   (UnitTSP, -- Teaspoon
    UnitSpec { unitSpecName       = "tsp"
             , unitSpecBase       = UnitL
             , unitSpecConversion = 0.00492892159
             , unitSpecDigits     = 2
             , unitSpecAliases    = [] }),
   (UnitTBSP, -- Tablespoon
    UnitSpec { unitSpecName       = "tbsp"
             , unitSpecBase       = UnitL
             , unitSpecConversion = 0.0147867648
             , unitSpecDigits     = 2
             , unitSpecAliases    = [] }),
   (UnitFLOZ, -- Fluid Ounce
    UnitSpec { unitSpecName       = "fl.oz"
             , unitSpecBase       = UnitL
             , unitSpecConversion = 0.0295735295625
             , unitSpecDigits     = 2
             , unitSpecAliases    = [] }),
   (UnitKG, -- Kilogram
    UnitSpec { unitSpecName       = "kg"
             , unitSpecBase       = UnitKG
             , unitSpecConversion = 1
             , unitSpecDigits     = 3
             , unitSpecAliases    = [] }),
   (UnitG, -- Gram
    UnitSpec { unitSpecName       = "g"
             , unitSpecBase       = UnitKG
             , unitSpecConversion = 0.001
             , unitSpecDigits     = 0
             , unitSpecAliases    = [] }),
   (UnitOZ, -- Ounce
    UnitSpec { unitSpecName       = "oz"
             , unitSpecBase       = UnitKG
             , unitSpecConversion = 0.0283495
             , unitSpecDigits     = 2
             , unitSpecAliases    = [] }),
   (UnitLB, -- Pound
    UnitSpec { unitSpecName       = "lb"
             , unitSpecBase       = UnitKG
             , unitSpecConversion = 0.45359237
             , unitSpecDigits     = 2
             , unitSpecAliases    = [] })]

-- | Given a Unit, lookup its base Unit (every Unit needs a base
-- Unit!).
lookupBaseUnit :: Unit -> Either String Unit
lookupBaseUnit u =
  maybeToEither errMsg (view base <$> lookupUnitSpec u)
  where errMsg = "Failed to lookup base unit for unit " ++ printUnit u

-- | Pretty printing for Units.
printUnit :: Unit -> String
printUnit u =
    case u of
      UnitNone -> ""
      UnitOther s -> s
      _ -> maybe "" (view name) $ M.lookup u _units

-- | Extract Units for a specified base Unit.
filterUnitsByBase :: Unit -> [Unit]
filterUnitsByBase u =
  _units^..ifolded.withIndex.filtered matchBase._1
  where matchBase (_, s) = s ^. base == u

-- | Given a Unit, lookup its UnitSpecification. Note that UnitOther
-- has no Unit specification, hence we need a Maybe type here.
lookupUnitSpec :: Unit -> Maybe UnitSpec
lookupUnitSpec = flip M.lookup _units

-- | Lookup the conversion factor for a given unit.
lookupConversionFactor :: Unit -> Either String Rational
lookupConversionFactor u =
  maybeToEither errMsg (view conversion <$> lookupUnitSpec u)
  where errMsg = "failed to lookup conversion factor for unit " ++ show u

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
