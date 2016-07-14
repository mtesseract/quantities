-- Quantities
-- Copyright (C) 2015-2016 Moritz Schulte <mtesseract@silverratio.net>

-- API is not necessarily stable.

{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE StandaloneDeriving     #-}

-- {-# LANGUAGE AllowAmbiguousTypes      #-}
-- {-# LANGUAGE FlexibleContexts       #-}
-- {-# LANGUAGE DataKinds       #-}

module Quantities
  ( UId(..)
  , UVolume(..)
  , UMass(..)
  , UKind(..)
  , UCustom(..)
  , Unit(..)
  , DerivedUnit(..)
  , Q(..)
  , Q'(..)
  , conversionFactor
  , mult
  , add
  , convert
  , parse
  , parseExt
  , conversion'
  ) where

import Data.Text.Lazy (Text)
import           Data.Char
import qualified Data.Text.Lazy as T
import           Formatting
import           Numeric (readFloat, readSigned)
import           Text.Read

data BaseUnitLiter

data BaseUMass

data UId = UId_L | UId_Ml | UId_Cup | UId_Tsp | UId_Tbsp | UId_FlOz
         | UId_Kg | UId_G | UId_Lb | UId_Oz
         | UId_Custom Text
  deriving (Eq, Show)

data UVolume = L | Ml | Cup | Tsp | Tbsp | FlOz
  deriving (Eq, Show, Read, Enum)

unitsVolume :: [UVolume]
unitsVolume = [L ..]

data UMass = Kg | G | Lb | Oz
  deriving (Eq, Show, Read, Enum)

data UKind = UKind_Volume | UKind_Mass | UKind_Custom
  deriving (Eq, Show)

unitsMass :: [UMass]
unitsMass = [Kg ..]

data UCustom = Custom Text
  deriving (Eq, Show)

class (Eq u, Show u) => Unit u where
  unitName'       :: u -> (Text, u)
  unitName        :: u -> Text
  units           :: [u]
  defaultUnit     :: u
  unitAliases'    :: u -> [(Text, u)]
  unitAliases     :: u -> [Text]
  unitNames'      :: u -> [(Text, u)]
  unitNames       :: u -> [Text]
  unitParse       :: Text -> Maybe u
  unitKind        :: u -> UKind
  maybeBaseFactor :: u -> Maybe Rational
  unitId          :: u -> UId

  -- Default Implementations:
  unitName u        = fst (unitName' u)
  unitAliases       = map fst . unitAliases'
  unitNames' u      = unitName' u : unitAliases' u
  unitNames         = map fst . unitNames'
  unitParse t       = lookup t (concatMap unitNames' units)
  maybeBaseFactor _ = Nothing

instance Unit UVolume where
  units           = unitsVolume
  unitName' u     = (specName (lookupUnit u), u)
  defaultUnit     = L
  unitAliases' u  = map (\ a -> (a, u)) (specAliases (lookupUnit u))
  unitKind _      = UKind_Volume
  maybeBaseFactor = Just . specFactor . lookupUnit
  unitId          = specUnitId . lookupUnit
  
instance Unit UMass where
  units           = unitsMass
  unitName' u     = (specName (lookupUnit u), u)
  defaultUnit     = Kg
  unitAliases' u  = map (\ a -> (a, u)) (specAliases (lookupUnit u))
  unitKind _      = UKind_Mass
  maybeBaseFactor = Just . specFactor . lookupUnit
  unitId          = specUnitId . lookupUnit

instance Unit UCustom where
  units                  = []
  unitName' u@(Custom s) = (s, u)
  defaultUnit            = Custom ""
  unitAliases' _         = []
  unitParse t            = Just (Custom t)
  unitKind _             = UKind_Custom
  unitId (Custom s)      = UId_Custom s

data Spec = Spec { specName    :: Text
                 , specUnitId  :: UId
                 , specFactor  :: Rational
                 , specAliases :: [Text]
                 } deriving (Show)

class Unit u => DerivedUnit b u | u -> b where
  derivedUnits :: b -> [u]
  similarUnits :: u -> [u]
  baseUnit     :: u -> u
  baseFactor   :: u -> Rational
  lookupUnit   :: u -> Spec

instance DerivedUnit BaseUnitLiter UVolume where
  similarUnits _ = unitsVolume
  derivedUnits _ = unitsVolume
  baseUnit _     = L
  baseFactor     = specFactor . lookupUnit
  lookupUnit u   = case u of
                     L    -> Spec "l"     UId_L       1               []
                     Ml   -> Spec "ml"    UId_Ml   1000               []
                     Cup  -> Spec "cup"   UId_Cup     0.236588236     ["cups"]
                     Tsp  -> Spec "tsp"   UId_Tsp     0.00492892159   []
                     Tbsp -> Spec "tbsp"  UId_Tbsp    0.0147867648    []
                     FlOz -> Spec "fl.oz" UId_FlOz    0.0295735295625 []
                   

instance DerivedUnit BaseUMass UMass where
  similarUnits _ = unitsMass
  derivedUnits _ = unitsMass
  baseUnit _     = Kg
  baseFactor     = specFactor . lookupUnit
  lookupUnit u   = case u of
                     Kg -> Spec "kg" UId_Kg 1          []
                     G  -> Spec "g"  UId_G  0.001      []
                     Lb -> Spec "lb" UId_Lb 0.45359237 []
                     Oz -> Spec "oz" UId_Oz 0.0283495  []

data Q u where
  Q :: Unit u => Rational -> u -> Q u

deriving instance Show (Q u)
deriving instance Eq (Q u)

data Q' where
  Q' :: Unit u => Rational -> u -> Q'

deriving instance Show Q'

instance Eq Q' where
  (Q' x u) == (Q' x' u') = x == x' && unitId u == unitId u'

class CompatibleUnits u u'

instance (DerivedUnit b u, DerivedUnit b u') => CompatibleUnits u u'

conversionFactor :: (DerivedUnit b u, DerivedUnit b u') => u -> u' -> Rational
conversionFactor u u' = baseFactor u / baseFactor u'

mult :: Unit u => Rational -> Q u -> Q u
mult λ (Q x u) = Q (λ * x) u

add :: (DerivedUnit b u, DerivedUnit b u') => Q u -> Q u' -> Q u
add (Q x u) (Q x' u') = Q (x + x' * (conversionFactor u u')) u
  
convert :: (DerivedUnit b u, DerivedUnit b u') => Q u -> u' -> Q u'
convert (Q x u) u' = Q (x * conversionFactor u u') u'

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
parseExt :: Unit u => (Text -> Either Text Rational) -> Text -> Either Text (Q u)
parseExt parser s = do
  let (w0, w1) = splitAtUnit s
  unit <- maybeToEither "Unknown Unit" (unitParse w1)
  num <- parser w0
  return $ Q num unit

  where splitAtUnit t =
          let num = T.takeWhile (not . isAlpha) t
              u   = T.drop (T.length num) t
          in (num, u)

-- | Parse a given quantity in its string representation, e.g. a
-- string of the form "0.7 l".
parse :: Unit u => Text -> Either Text (Q u)
parse = parseExt parseNumber

-------------------------------
-- General Utility Functions --
-------------------------------

-- | Utility function for converting Maybe values to Either values.
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing  = Left  a

-- | Return first Right value in the list. If the list contains to
-- Right, return default Left.
eitherAlternative :: a -> [Either a b] -> Either a b
eitherAlternative def []             = Left def
eitherAlternative _   (Right x : _)  = Right x
eitherAlternative def (Left _  : xs) = eitherAlternative def xs

conversion' :: Q' -> Q' -> Maybe Rational
conversion' (Q' x1 u1) (Q' x2 u2) = do
  let uKind1 = unitKind u1
      uKind2 = unitKind u2
  _ <- if uKind1 == uKind2 then return () else Nothing
  factor1 <- maybeBaseFactor u1
  factor2 <- maybeBaseFactor u2
  return $ (x1 * factor1) / (x2 * factor2)
