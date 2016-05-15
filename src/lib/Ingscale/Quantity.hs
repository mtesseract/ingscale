module Ingscale.Quantity where

import           Control.Lens
import           Control.Monad (when)
import           Data.Either
import           Data.Function (on)
import           Data.List (sortBy)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Ratio
import           Ingscale.Types
import           Ingscale.Units

-- | Given a Quantity, Compute the list of "equivalent Quantities".
equivalentQuantities :: Quantity -> [Quantity]
equivalentQuantities q =
  case lookupBaseUnit (q^.unit) of
    Left _  ->
      []
    Right b ->
      filterUnitsByBase b ^.. traverse.filtered (/= q^.unit) &
      rights . map (convertQuantity q)


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

-- | Try to round a given quantity, using the unitSpecRound field
-- contained in the UnitSpecification. If the quantity's denominator
-- is contained in goodDeminator, return the number unmodified.  If
-- for some reason the UnitSpecification cannot be found, return the
-- number unmodified.
roundQuantity :: QuantityTransformer
roundQuantity q =
  if denominator (q^.number) `elem` goodDenominators
     then q
     else let maybeSpec = M.lookup (q ^. unit) _units
              nDigits   = fromMaybe defaultNDigits (view digits <$> maybeSpec)
          in q & number %~ roundIt nDigits

  where roundIt nDigits num =
          fromIntegral (round (num * 10^^nDigits) :: Integer) / 10^^nDigits

        defaultNDigits :: Int
        defaultNDigits = 2

-- | Quantity Transformer: Approximate in a clever way.
approximateQuantity :: Rational -> QuantityTransformer
approximateQuantity epsilon = number %~ approximateNumber epsilon


-- | If possible, convert a given Quantity into a new Quantity using
-- the specified Unit. Returns Nothing if the Unit of the specified
-- Quantity cannot be converted to the specified Unit.
convertQuantity :: Quantity -> Unit -> Either String Quantity
convertQuantity q toUnit = do
  factor <- conversionFactor (q^.unit) toUnit
  return (q & number //~ factor
            & unit .~ toUnit)

-- quantity1 * computeScalingFactor' = quantity2
computeScalingFactorQuantity :: Quantity -> Quantity -> Either String Rational
computeScalingFactorQuantity q1 q2 = do
  when (q1^.number == 0) $ Left "Quantity is zero."
  conversionUnit <- conversionFactor (q1^.unit) (q2^.unit)
  return $ conversionUnit * (q2^.number / q1^.number)
