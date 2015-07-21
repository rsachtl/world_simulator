{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module AsmRunner where

import           Control.Applicative
import           Data.List           as List


data Update c i where
  Update :: (AsmContext c i) => i -> (c -> c) -> Update c i

instance Eq (Update c i) where
  (Update i1 _) == (Update i2 _) = i1 == i2

instance Ord (Update c i) where
  (Update i1 _) <= (Update i2 _) = i1 <= i2

applyUpdate :: (AsmContext c i) => c -> Update c i -> c
applyUpdate c (Update _ f) = f c


data ASM c i where
  ASM :: (AsmContext c i) => i -> (i -> c -> Update c i) -> ASM c i

runASM :: (AsmContext c i) => c -> ASM c i -> Update c i
runASM c (ASM i f) = f i c

-- i1 > i2 === i1 to be performed earlier
class (Ord i) => AsmContext c i where
  step :: c -> [ASM c i] -> c
  step c asms = case combineUpdates updates of
    Just u  -> applyUpdate c u
    Nothing -> c
    where
      updates :: [Update c i]
      updates = runASM c <$> asms ++ getInnerASMs c

  stepInner :: c -> c
  stepInner c = case combineUpdates updates of
    Just u  -> applyUpdate c u
    Nothing -> c
    where
      updates :: [Update c i]
      updates = runASM c <$> getInnerASMs c

  getInnerASMs :: c -> [ASM c i]
  getInnerASMs _ = []

  combineUpdatePair :: Update c i -> Update c i -> Update c i
  combineUpdatePair (Update i1 f) (Update i2 g) = if i1 > i2
    then Update i1 (g . f)
    else Update i2 (f . g)

  combineUpdates :: [Update c i] -> Maybe (Update c i)
  combineUpdates us = if not (null us)
    then Just $ foldl1 combineUpdatePair $ List.sort us
    else Nothing
