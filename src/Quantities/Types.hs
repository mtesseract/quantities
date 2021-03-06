-- Quantities
-- Copyright (C) 2015-2016 Moritz Schulte <mtesseract@silverratio.net>

-- API is not necessarily stable.

{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs       #-}
{-# LANGUAGE StandaloneDeriving     #-}

module Quantities.Types
       ( Unit(..)
       , UnitSpec(..)
       , Quantity(..)
       , QuantityTransformer
       , name
       , base
       , conversion
       , aliases
       , number
       , unit
       , HasName
       , HasBase
       , HasConversion
       , HasAliases
       , HasNumber
       , HasUnit
       ) where

import Control.Lens (makeFields)
import Data.Text.Lazy (Text)

---------------
-- Datatypes --
---------------

-- | These are the supported units. UnitNone is for the degenerate
-- case of missing unit.
data Unit = UnitNone
          | UnitL
          | UnitML
          | UnitCup
          | UnitTSP
          | UnitTBSP
          | UnitFLOZ
          | UnitG
          | UnitKG
          | UnitLB
          | UnitOZ
          | UnitOther Text deriving (Eq, Show, Read, Ord)

-- | A UnitSpec datatype defines a unit completely.
data UnitSpec = UnitSpec
  { unitSpecName       :: Text     -- Abbreviation of this unit.
  , unitSpecBase       :: Unit     -- The unit in which terms this unit is defined.
  , unitSpecConversion :: Rational -- Conversion factor from this unit to the base unit.
  , unitSpecAliases    :: [Text]   -- A list of abbreviation aliases for this unit.
  } deriving (Show)

makeFields ''UnitSpec

-- | A Quantity is the combination of a (rational) number and a Unit.
data Quantity = Quantity {quantityNumber :: Rational, quantityUnit :: Unit} deriving (Eq, Show)

makeFields ''Quantity

type QuantityTransformer = Quantity -> Quantity
