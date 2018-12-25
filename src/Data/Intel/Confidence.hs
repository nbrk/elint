module Data.Intel.Confidence where

-- | Analytic confidence of the information
data Confidence
  = High
  | Moderate
  | Low
  | Probability Float
  deriving (Show)


instance Bounded Confidence where
  minBound = Low
  maxBound = High


instance Enum Confidence where
  fromEnum Low = 1
  fromEnum Moderate = 2
  fromEnum High = 3
  fromEnum (Probability f) | f <= 0 = 1 -- considered Low
                           | f > 0 && f <= 1/3 = 1
                           | f > 1/3 && f <= 2/3 = 2
                           | f > 2/3 && f <= 1 = 3
                           | f > 1 = 3 -- considered High
  toEnum i | i <= 1 = Low
           | i == 2 = Moderate
           | i >= 3 = High

instance Eq Confidence where
  (Probability f1) == (Probability f2) = f1 == f2
  c1 == c2 = fromEnum c1 == fromEnum c2


instance Ord Confidence where
  (Probability f1) <= (Probability f2) = f1 <= f2
  c1 <= c2 = fromEnum c1 <= fromEnum c2
