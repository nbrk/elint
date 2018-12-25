module Data.Intel
  ( Confidence(..)
  , Source
  , Age
  , IntelLens
  , Intel(..)
  , IntelOn(..)
  , IntelDB(..)

  , makeIntel
  , makeIntelHelpers
  , makeIntelDBFor
  , makeIntelLensesFor

  , noIntel
  , chooseIntel
  , obsolete
  , incorp
  , incorpBest
  , incorpBests
  , increaseIntelAge
  , query
  , queryData
  ) where

import           Data.Intel.Confidence
import           Data.Intel.Intel
import           Data.Intel.TH
