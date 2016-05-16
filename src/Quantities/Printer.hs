-- Quantities
-- Copyright (C) 2015-2016 Moritz Schulte <mtesseract@silverratio.net>

-- API is not necessarily stable.

{-# LANGUAGE OverloadedStrings #-}

module Quantities.Printer
       ( printQuantity
       , printMixed
       , printFloat
       , printRational
       ) where

import           Control.Lens
import           Data.Ratio (numerator, denominator)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Formatting
import           Quantities.Types
import           Quantities.Units
import           Quantities.Util

---------------------
-- Pretty Printers --
---------------------

printRational :: Rational -> Text
printRational x =
  let n = numerator   x
      d = denominator x
  in format (int % "/" % int) n d

printFloat :: Rational -> Text  
printFloat x = format float x

-- | Convert a rational number into its string representation as a
-- mixed number; e.g. 1 % 2 => "1/2".
printMixed :: Rational -> Text
printMixed x' =
    let x      = abs x'          -- Absolute Value of x.
        minus  = x' < 0          -- Sign of x.
        num    = numerator x     -- Numerator of x.
        denom  = denominator x   -- Denominator of x.
        i      = num `div` denom -- Number of times the denominator
                                 -- fits into the numerator...
        rest   = num `mod` denom -- ... and the rest.
        prefix = if i == 0
                     -- If num < denom, then we surely do not have an
                     -- integer prefix.
                     then if rest == 0
                             -- If also the rest is zero, then "0" is
                             -- the final result. Otherwise we simply
                             -- have an empty prefix.
                             then "0"
                             else ""
                     else showText i
        fraction = if rest == 0
                      then ""
                      else format (int % "/" % int) rest denom
    in -- Assemble the final string:
       format (text % text % text % text)
               -- Handle minus sign:
               (if minus then "-" else "")
               -- Then the integer prefix:
               prefix
               -- If prefix and fraction is both non-empty, then we
               -- need to add a blank:
               (if T.null prefix || T.null fraction then "" else " ")
               -- And finally the fraction:
               fraction

-- | Pretty print a Quantity.
printQuantity :: (Rational -> Text) -> Quantity -> Text
printQuantity printer q =
  format (text % " " % text) (printer (q ^. number)) (printUnit (q ^. unit))
