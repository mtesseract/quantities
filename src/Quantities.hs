-- Quantities
-- Copyright (C) 2015-2016 Moritz Schulte <mtesseract@silverratio.net>

-- API is not necessarily stable.

{-# LANGUAGE OverloadedStrings #-}

module Quantities
       ( Unit(..)
       , Quantity(..)
       , conversionFactor
       , convertQuantity
       , equivalentQuantities
       , lookupBaseUnit
       , lookupUnitsByBase
       , parseNumber
       , parseQuantity
       , roundQuantity
       , approximateQuantity
       , printQuantity
       , computeScalingFactor
       ) where

import           Control.Lens
import           Control.Monad (when)
import           Data.Either
import           Data.Text.Lazy (Text)
import           Quantities.Parser
import           Quantities.Printer
import           Quantities.Rounding
import           Quantities.Types
import           Quantities.Units

-- | Given a Quantity, Compute the list of "equivalent Quantities".
equivalentQuantities :: Quantity -> [Quantity]
equivalentQuantities q =
  case lookupBaseUnit (q^.unit) of
    Left _  ->
      []
    Right b ->
      filterUnitsByBase b ^.. traverse.filtered (/= q^.unit) &
      rights . map (convertQuantity q)

------------------------------------------------------
-- Quantity Transformers (Rounding & Approximation) --
------------------------------------------------------

-- | If possible, convert a given Quantity into a new Quantity using
-- the specified Unit. Returns Nothing if the Unit of the specified
-- Quantity cannot be converted to the specified Unit.
convertQuantity :: Quantity -> Unit -> Either Text Quantity
convertQuantity q toUnit = do
  factor <- conversionFactor (q^.unit) toUnit
  return (q & number //~ factor
            & unit .~ toUnit)

-- quantity1 * computeScalingFactor' = quantity2
computeScalingFactor :: Quantity -> Quantity -> Either Text Rational
computeScalingFactor q1 q2 = do
  when (q1^.number == 0) $ Left "Quantity is zero."
  conversionUnit <- conversionFactor (q1^.unit) (q2^.unit)
  return $ conversionUnit * (q2^.number / q1^.number)
