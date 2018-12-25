{-# LANGUAGE TemplateHaskell #-}
module Example2 where

import           Data.Intel

data Type = Infantry | Armor | Mechanized | Recon | Artillery | HQ
data Posture = Moving | Stationary | Entrenched
data Echelon = Squad | Platoon | Company | Battalion | Brigade | Division | Corps | Army

data Unit = Unit
  { unitType     :: Type
  , unitEchelon  :: Echelon
  , unitLocation :: (Double, Double)
  , unitPosture  :: Posture
  , unitName     :: String
  , unitHQ       :: Unit
  }

makeIntel ''Unit
makeIntelHelpers ''Unit

