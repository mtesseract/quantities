-- Quantities
-- Copyright (C) 2015-2016 Moritz Schulte <mtesseract@silverratio.net>

-- API is not necessarily stable.

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Quantities.Parser
       ( parseNumber
       , parseFraction
       , parseInt
       , parseDecimal
       , parseMixed
       , parseQuantity
       ) where

import           Data.Char
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Formatting
import           Numeric (readFloat, readSigned)
import           Quantities.Types
import           Quantities.Units
import           Quantities.Util
import           Text.Read

-------------
-- Parsers --
-------------

-- | Try to parse an Integer.
parseInt :: Text -> Either Text Integer
parseInt s =
  let result = readMaybe (T.unpack s) :: Maybe Integer
  in case result of
       Nothing -> Left $ format ("Failed to parse integer `" % text % "'") s
       Just x  -> Right x

-- | Try to parse a fraction of the form "x/y" into a Rational.
parseFraction :: Text -> Either Text Rational
parseFraction s =
  let result = readMaybe (T.unpack (T.replace "/" "%" s))
  in case result of
       Nothing -> Left $ format ("Failed to parse fraction `" % text % "'") s
       Just x  -> Right x

-- | Try to parse a decimal number.
parseDecimal :: Text -> Either Text Rational
parseDecimal s =
  let result = readSigned readFloat (T.unpack s) :: [(Rational, String)]
  in case result of
    [(x, "")] -> Right x
    _         -> Left $ format ("Failed to parse decimal number `" % text % "'") s

-- | Try to parse a mixed number, e.g. a number of the form "5 23/42".
parseMixed :: Text -> Either Text Rational
parseMixed s =
  let components = T.words s
  in case components of
    [c0] ->
      if contains '/' c0
      then parseFraction c0
      else fromInteger <$> parseInt c0
    [c0, c1] -> do
      i <- parseInt c0
      frac <- parseFraction c1
      case (i < 0, frac < 0) of
        (False, False) -> Right $ fromInteger i + frac
        (True,  False) -> Right $ -1 * (fromInteger (abs i) + frac)
        (False, True)  -> Left errNoParse
        (True,  True)  -> Left errNoParse
    _ -> Left errNoParse
  where errNoParse = "No Parse"
        contains c t = (not . T.null . T.filter (== c)) t

-- | Try to parse a given number in string representation. First try
-- to parse it as a decimal number and if that fails, try to parse it
-- as a mixed number.
parseNumber :: Text -> Either Text Rational
parseNumber s' =
  let s = T.strip s'
  in eitherAlternative "Failed to parse number"
       [parseDecimal s, parseMixed s]

-- | Parse a given quantity in its string representation, e.g. a
-- string of the form "0.7 l".
parseQuantity :: (Text -> Either Text Rational) -> Text -> Either Text Quantity
parseQuantity parser s = do
  let (w0, w1) = splitAtUnit s
      u        = stringToUnit w1
  num <- parser w0
  return $ Quantity num u

  where splitAtUnit t =
          let num = T.takeWhile (not . isAlpha) t
              u   = T.drop (T.length num) t
          in (num, u)
