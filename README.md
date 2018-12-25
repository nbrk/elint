# ELINT (ELectronic INTelligence)

`elint` is a Haskell library that deals with uncertain and incomplete
information in a type-safe manner.

The library makes it easier and safer to manage various *intelligence data* (as
in *military intelligence*) properties such as:

- incompleteness of information (when only parts of the whole are known)
- uncertainty of information (analytic confidence statements or
  probabilities)
- degradation (aging) of the information
- priorities and choices between similar intelligence reports (deduction of the
  best report among all available)
- bulk-processing and incorporation of heterogenous intelligence data

## Idea
In a nutshell, the main idea is as follows: given a product type `Person`

``` haskell
data Person = Person
  { personAge  :: Int
  , personName :: String
  }
```

which is a *real model* of some enitity, derive the following related
structures:

1. Type `PersonIntel` (which is the *intelligence model* of the entity
   `Person`). We call `PersonIntel` an *intel database* as it holds type `Intel
   w a` for every record selector of the original type:

   ``` haskell
   data PersonIntel = PersonIntel
   { _personAgeIntel :: Intel PersonIntel Int
   , _personNameIntel :: Intel PersonIntel String
   }
   ```

2. Lenses of type `Lens' PersonIntel i` (which we conveniently synonym to
   `IntelLens w i`) for the generated type `PersonIntel` mirroring the record
   selectors:

  ``` haskell
  personAgeIntel :: Lens' PersonIntel (Intel PersonIntel Int)
  personNameIntel :: Lens' PersonIntel (Intel PersonIntel String)
  ```

3. Instance `instance IntelDB PersonIntel` which has the following methods for
   getting initially unknown database, and for aging the whole intelligence
   model one step (aging time is abstracted to `Integer`):

   ``` haskell
  class IntelDB w where
  mystery :: w
  age :: w -> w
  ```

From now on, the user is able to use various library functions to add
(incorporate) or delete (obsolete) `Intel PersonIntel a` reports from the
`PersonIntel` intelligence database. Main functions are:

``` haskell
incorp :: Intel w a -> w -> w
obsolete :: IntelLens w a -> w -> w
queryData :: IntelLens w a -> w -> Maybe a
```

The type-safety won't allow us to accidentely mix `Intel w a` reports from
different intelligence database (like `PersonIntel` and `CompanyIntel`) or
reports on different subjects (subdata pointed by `IntelLens`) within the same
database. Every `Intel w a` structure contains relevant `IntelLens` that
unambiguously associates the report with the subdata within the intel db:

``` haskell
data Intel w a = Intel
  { intelLens       :: IntelLens w a
  , intelData       :: a
  , intelConfidence :: Confidence
  , intelSource     :: Source
  , intelAge        :: Age
  }
  | NoIntel
```

## Confidence
Analytic confidence is modelled somewhat roughly:

``` haskell
data Confidence
  = High
  | Moderate
  | Low
  | Probability Float


instance Bounded Confidence where
-- ...

instance Enum Confidence where
-- ...

instance Eq Confidence where
-- ...

instance Ord Confidence where
-- ...
```

Basic statements are `Low`, `Moderate` and `High`. They are mapped to floating
[0, 1] as one-thirds of the interval, so you can compare both probability values
and probability VS level values (i.e. `Probability 0.3` VS `Moderate`). The
comparison is hard-coded (fresh is best, confident is best) and is used when
incorporating intel reports into the database.

## Degradation of intel information with time
Typeclass `IntelDB` features method `age` which models incremental aging of all
`Intel w a` reports in the database. The endomorphism increments ages of all
current intel reports by one. When a new report is inserted, its age is
automatically reset. Values of age are taken into the account when two or more
reports are compared in the process of incorporation.

## Incompleteness of information
When you obtain a void database with `mystery`, the intel information is
perfectly incomplete: targets of all `IntelLens`es are initially set to
`NoIntel`. When you get new sightings (i.e. `Intel w a`) of an enemy infantry
unit, you continously incorporate them in, eventually having some or all of the
subdata contain *something*.

## Usage example
Usually, you want to use the library's TemplateHaskell helpers to generate the
types, instances and lenses:

``` haskell
{-# LANGUAGE TemplateHaskell #-}
module Example1 where

import           Data.Intel

data Person = Person
  { personAge  :: Int
  , personName :: String
  }

makeIntel ''Person
makeIntelHelpers ''Person
```

In GHCi:

``` haskell
> let ni1 = Intel personNameIntel "Alexander" (Probability 0.5) "" 0
> let ni2 = Intel personNameIntel "Nikolay" High "" 0
> queryData personNameIntel $ incorpBest [ni1, ni2] mystery
Just "Nikolay"
> queryData personAgeIntel $ incorpBest [ni1, ni2] mystery
Nothing
>
> -- multiple age sightings ...
> let ai1 = Intel personAgeIntel 30 Moderate "" 0
> let ai2 = Intel personAgeIntel 31 High "" 0
> let ai3 = Intel personAgeIntel 32 Low "" 0
> queryData personAgeIntel $ incorpBest [ai1, ai2, ai3] mystery
Just 31
```

## Heterogenous incorporation
You can generalize reports on different subjects within the same database if
you wrap `Intel w a` into the following existential type:

``` haskell
data IntelOn w =
  forall a . IntelOn (Intel w a)
```

Then you can freely incorporate all the (heterogenous) reports into your
database using `incorpBests :: [IntelOn w] -> w -> w`. Because every report
contains relevant `IntelLens` (and `NoIntel` is identity), the types would match
meaning you can handle the reconnaissance from above just like this:

``` haskell
> let someintel = map IntelOn [ni1, ni2] ++ map IntelOn [ai1, ai2, ai3]
> :t someintel
someintel :: [IntelOn PersonIntel]
> queryData personAgeIntel $ incorpBests someintel mystery
Just 31
> queryData personNameIntel $ incorpBests someintel mystery
Just "Nikolay"
```
