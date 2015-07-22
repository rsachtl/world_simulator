{-# LANGUAGE MultiParamTypeClasses #-}

module SimpleWorld where

import           AsmRunner
import           Data.Map  as Map
import           Entities
import Region

-- run entire environment
environmentF :: StepOrdering -> World -> Update World StepOrdering
environmentF i w = Update i id

-- run single region
regionF :: RegionId -> StepOrdering -> World -> Update World StepOrdering
regionF rId i w = Update i (\w -> w {regions = insert rId newReg $ regions w} )
  where
    reg :: Region
    reg = regions w ! rId
    newReg :: Region
    newReg = reg {population = 0}

-- run entire civilization (civil)
civilF :: NationId -> StepOrdering -> World -> Update World StepOrdering
civilF nId i w = Update i id

-- run entire civilization (military)
militaryF :: NationId -> StepOrdering -> World -> Update World StepOrdering
militaryF nId i w = Update i id

-- run single Army
armyF :: NationId -> RegionId -> StepOrdering -> World -> Update World StepOrdering
armyF nId rId i w = Update i id




getRegionFs :: World -> [WorldASM]
getRegionFs w = fmap (ASM (Environment 0) . regionF) $ Map.keys $ regions w

getCivilFs :: World -> [WorldASM]
getCivilFs w = fmap (ASM (Civil 0) . civilF) $ Map.keys $ nations w

getMilitaryFs :: World -> [WorldASM]
getMilitaryFs w = fmap (ASM (Military 0) . militaryF) $ Map.keys $ nations w

getArmyFs :: World -> [WorldASM]
getArmyFs w = fmap (\(nId,rId) -> ASM (Military 1) (armyF nId rId)) locs
  where
    nIds :: [NationId]
    nIds = Map.keys $ nations w
    rIds :: NationId -> [RegionId]
    rIds nId = Map.keys $ forces $ nations w ! nId
    locs :: [(NationId,RegionId)]
    locs = [(nId,rId) | nId <- nIds, rId <- rIds nId]

instance AsmContext World StepOrdering where
  getInnerASMs w = getRegionFs w ++ getArmyFs w ++ getCivilFs w

{------ Example ------}

regionT1 :: Region
regionT1 = Region
  { population     = 1000
  , industry       = 5
  , infrastructure = 10
  , devastation    = 0
  , happiness      = 10
  }

regionT2 :: Region
regionT2 = Region
  { population     = 2900
  , industry       = 29
  , infrastructure = 50
  , devastation    = 0
  , happiness      = 8
  }

regionT3 :: Region
regionT3 = Region
  { population     = 650
  , industry       = 0
  , infrastructure = 2
  , devastation    = 0
  , happiness      = 0
  }

armyT1 :: Army
armyT1 = Army
  { strength     = 100
  , morale       = 10
  , organization = 10
  }

armyT2 :: Army
armyT2 = Army
  { strength     = 30
  , morale       = 15
  , organization = 5
  }

armyT3 :: Army
armyT3 = Army
  { strength     = 120
  , morale       = 5
  , organization = 10
  }

nationT1 :: Nation
nationT1 = Nation
  { controlledRegions = [1]
  , forces            = Map.fromList [(1,armyT1)]
  , enemies           = [2]
  , allies            = []
  }

nationT2 :: Nation
nationT2 = Nation
  { controlledRegions = [3]
  , forces            = Map.fromList [(2,armyT2),(3,armyT2)]
  , enemies           = [1]
  , allies            = []
  }

worldT1 :: World
worldT1 = World
  { regions = Map.fromList [(1,regionT1),(2,regionT2),(3,regionT3)]
  , nations = Map.fromList [(1,nationT1),(2,nationT2)]
  }

worldStep :: World
worldStep = step worldT1 ([] :: [ASM World StepOrdering])
