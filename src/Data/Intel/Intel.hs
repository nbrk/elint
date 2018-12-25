{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
module Data.Intel.Intel where

import           Data.Intel.Confidence

import           Control.Lens


-- | Source of the information
type Source = String


-- | Abstract age of the information: the smaller the newer
type Age = Integer


-- | Intelligence report on the subject (the lens over w)
data Intel w a = Intel
  { intelLens       :: IntelLens w a
  , intelData       :: a
  , intelConfidence :: Confidence
  , intelSource     :: Source
  , intelAge        :: Age
  }
  | NoIntel


-- | Intelligence report on any subject within given Intel db
data IntelOn w =
  forall a . IntelOn (Intel w a)


-- | Consider two intel reports equal iff they are of
-- the same age and of the same confidence level (the
-- data and the IntelLens do not matter)
instance Eq (Intel w a) where
  NoIntel == NoIntel = True
  i1 == i2 | intelAge i1 == intelAge i2 &&
             intelConfidence i1 == intelConfidence i2 =
               True
           | otherwise = False


-- | Intelligence priority over the intel reports.
-- We prefer fresh one over the old one. We prefer most
-- confident if the freshness is same. We prefer first one
-- if the ages and confidence levels are same (i1 == i2).
instance Semigroup (Intel w a) where
  i <> NoIntel = i
  NoIntel <> i = i
  i1 <> i2 | i1 == i2 = i1 -- doesn't matter
  i1 <> i2 =
    case intelAge i1 `compare` intelAge i2 of
      -- always choose the newest one
      LT -> i1
      GT -> i2
      EQ ->
        case
          intelConfidence i1 `compare` intelConfidence i2
        of
          -- Always choose the most confident one when
          -- ages are the same.
          LT -> i2
          GT -> i1
          EQ -> i1 -- should not happen (i1 == i2)

-- | Absence of intelligence information block (no report)
instance Monoid (Intel w a) where
  mempty = NoIntel


-- | Lens (i.e. a setter/getter) inside the XXXIntel type
type IntelLens w a = Lens' w (Intel w a)


-- | Collections of intel reports: a void collection,
-- aging of the collection.
class IntelDB w where
  mystery :: w
  age :: w -> w


-- | Absence of the intelligence (i.e. on an IntelLens)
noIntel :: Intel w a
noIntel = mempty


-- | Increase age of the whole intelligence database
increaseIntelAge :: Intel w a -> Intel w a
increaseIntelAge NoIntel = NoIntel
increaseIntelAge i =
  let age = intelAge i
  in
    i { intelAge = age + 1 }


-- | Choose the best of two intelligence reports
chooseIntel :: Intel w a -> Intel w a -> Intel w a
chooseIntel = (<>)


-- | Incorporate a single Intel into the intelligence db
incorp :: Intel w a -> w -> w
incorp NoIntel w = w
incorp i w =
  let l = intelLens i
  in
    -- choose between given and current intel reports
    w & l %~ chooseIntel i


-- | Incorporate a list of similar (same IntelLens) Intel
-- reports choosing the best one among all
incorpBest :: [Intel w a] -> w -> w
incorpBest is w = foldr incorp w is


-- | Incorporate a list of reports that concern this very
-- Intel db in general (hence, `Intel w a` becomes
-- `IntelOn w` by existentials)
incorpBests :: [IntelOn w] -> w -> w
incorpBests is w = foldr (\(IntelOn i) -> incorp i) w is


-- | Obsolete (delete) current intel pointed by IntelLens
obsolete :: IntelLens w a -> w -> w
obsolete l = set l noIntel


-- | Query the intel db given the IntelLens
query :: IntelLens w a -> w -> Maybe (a, Confidence, Source, Age)
query l w =
  let i = w ^. l
  in
    case i of
      NoIntel -> Nothing
      i -> Just
        ( intelData i, intelConfidence i
        , intelSource i, intelAge i)


-- | Extract only the data from the intel db
queryData :: IntelLens w a -> w -> Maybe a
queryData l w = fmap d (query l w)
  where d (a, _, _, _) = a




