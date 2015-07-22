module Entities where

import           AsmRunner
import Region
import           Data.Map  as Map

type RegionId = Int
type NationId = Int
type ArmyId = Int
type WorldASM = ASM World StepOrdering

data StepOrdering =
    Military Int
  | Civil Int
  | Environment Int
  deriving Eq

instance Ord StepOrdering where
  (Military m)    <= (Military n)    = m <= n
  (Civil _)       <= (Military _)    = True
  (Environment _) <= (Military _)    = True
  (Military _)    <= (Civil _)       = False
  (Civil m)       <= (Civil n)       = m <= n
  (Environment _) <= (Civil _)       = True
  (Military _)    <= (Environment _) = False
  (Civil _)       <= (Environment _) = False
  (Environment m) <= (Environment n) = n <= m

data World = World
  { regions :: Map RegionId Region
  , nations :: Map NationId Nation
  } deriving Show

data Nation = Nation
  { controlledRegions :: [RegionId]
  , forces            :: Map Int Army
  , enemies           :: [NationId]
  , allies            :: [NationId]
  } deriving Show

data Army = Army
  { strength     :: Int
  , morale       :: Int
  , organization :: Int
  } deriving Show
