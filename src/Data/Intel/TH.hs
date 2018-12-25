{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Intel.TH
  ( makeIntel
  , makeIntelHelpers
  , makeIntelDBFor
  , makeIntelLensesFor
  ) where

import           Data.Intel.Intel

import           Control.Lens
import           Language.Haskell.TH


data T1 = T1

forcePrefix :: Char -> String -> String
forcePrefix c (x:xs) | x == c = x:xs
                      | otherwise = c:x:xs

constructTypeName :: Name -> Name
constructTypeName name =
  mkName $ nameBase name ++ "Intel"


-- makeAll name = do
--   d1 <- makeIntel name
--   d2 <- makeIntelHelpers name
--   return $ d1 ++ d2

makeIntel :: Name -> Q [Dec]
makeIntel name = do
  TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify name
  let tyname = constructTypeName name
  let mkproffield (n, b, t) =
        ( mkName (forcePrefix '_' (nameBase n) ++ "Intel")
        , b
        , let t1 = AppT
                   (ConT (mkName "Intel"))
                   (ConT tyname)
              t2 = AppT t1 t
--              t3 = AppT (ConT (mkName "Maybe")) t2
          in
--            t3
            t2
        )

  let proffields = map mkproffield fields
  return [ DataD
           []
           tyname
           []
           Nothing
           [ RecC
             tyname
             proffields ]
           []
         ]

makeIntelHelpers :: Name -> Q [Dec]
makeIntelHelpers name  = do
  let n = constructTypeName name
  d1 <- makeIntelDBFor n
  d2 <- makeIntelLensesFor n
  return $ d1 ++ d2


makeIntelLensesFor :: Name -> Q [Dec]
makeIntelLensesFor =
  makeLenses


-- T noIntel noIntel ... noIntel
genMysteryFuncExp :: Name -> Int -> Q Exp
genMysteryFuncExp name i = do
  let args = replicate i (VarE (mkName "noIntel"))
  return $
    foldr (flip AppE) (ConE name) args


-- record update:  { foo = f (foo t), bar = f (bar t), ... }
genAgeFuncExp :: Name -> [Name] -> Q Exp
genAgeFuncExp name fields = do
   let fieldexps =
         map
         (\n -> (n,
                       AppE
                       (VarE (mkName "increaseIntelAge"))
                       (AppE (VarE n) (VarE (mkName "r")))
                       ))
         fields
   return $
     RecUpdE
     (VarE (mkName "r"))
     fieldexps


makeIntelDBFor :: Name -> Q [Dec]
makeIntelDBFor name = do
  TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify name

  d <- [d|
        instance IntelDB T1 where
          mystery =
            $(genMysteryFuncExp name (length fields))
          age r =
            $(genAgeFuncExp name ((\(a,_,_) -> a) (unzip3 fields)))
--              where increaseIntelAgeMaybe = fmap increaseIntelAge

       |]
  let [InstanceD Nothing [] (AppT (ConT c) (ConT _T1)) fs] = d

  return [InstanceD Nothing [] (AppT (ConT c) (ConT name)) fs]

