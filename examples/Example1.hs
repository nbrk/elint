{-# LANGUAGE TemplateHaskell #-}
module Example1 where

import           Data.Intel


data Person = Person
  { personAge  :: Int
  , personName :: String
  } deriving (Show)


makeIntel ''Person
makeIntelHelpers ''Person

