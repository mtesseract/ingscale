-- IngScale - Library for convenient scaling of ingredients lists.
-- Copyright (C) 2015-2016 Moritz Schulte <mtesseract@silverratio.net>

-- API is not necessarily stable.

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
                 scaleIngredients,
                 printQuantity,
                 printIngredient,
                 printIngredients,
                 printIngredientsExt) where

import Control.Lens
import Data.Maybe (listToMaybe)
import Ingscale.Parser
import Ingscale.Printer
import Ingscale.Quantity
import Ingscale.Types
import Ingscale.Units
import Ingscale.Util

-----------------------------------------
-- Ingscale Specific Utility Functions --
-----------------------------------------

-- | Given an ingredients list INGREDIENTS and a NAME, try to extract
-- the single ingredient by that name. Returns Nothing if INGREDIENTS
-- does not contain an ingredient by that name.
extractIngredient :: [Ingredient] -> String -> Maybe Ingredient
extractIngredient ingredients n =
  listToMaybe $ filter ((==) n . view name) ingredients

-- | Like extractIngredient, but do not compute the complete
-- Ingredient datatype, return only its contained Quantity.
extractIngredientQuantity :: [Ingredient] -> String -> Maybe Quantity
extractIngredientQuantity ingredients n =
 view quantity <$> extractIngredient ingredients n

-------------
-- Scaling --
-------------

-- | Quantity Transformer: Scale a given Quantity by a given factor.
scaleQuantity :: Rational -> QuantityTransformer
scaleQuantity factor = number *~ factor

-- | Scale the ingredients list INGREDIENTS by the factor FACTOR.
scaleIngredients :: Rational -> [Ingredient] -> [Ingredient]
scaleIngredients factor =
  traverse . quantity %~ (roundQuantity . scaleQuantity factor)

-- | Given a list of INGREDIENTS and another INGREDIENT, compute the
-- factor by which the ingredients list has to be scaled such that the
-- ingredient with the same name as INGREDIENT contained in
-- INGREDIENTS has exactly the quantity of INGREDIENT.
computeScalingFactor :: [Ingredient] -> Ingredient -> Either String Rational
computeScalingFactor ingredients i = do
  q1 <- maybeToEither "Ingredient not found in list" $
    extractIngredientQuantity ingredients (i ^. name)
  let q2 = i ^. quantity
  computeScalingFactorQuantity q1 q2
