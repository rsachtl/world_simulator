module Region where

import Data.Map as Map

type RegionId = Int
type NationId = Int
type StepCount = Int

data Region = Region
  { regionId        :: RegionId
  , ownerId         :: NationId
  , allegiance      :: NationId
  , ownershipDuration :: StepCount
  , adjacentRegions :: [RegionId]
  , population      :: Int
  , industry        :: Int
  , infrastructure  :: Int
  , devastation     :: Int
  , happiness       :: Int
  , terrain :: Terrain
  , terrainFeatures :: [TerrainFeature]
  , ressources :: Map Ressource Int
  } deriving Show

data Terrain =
    Plain
  | Rough
  | Hills
  | Desert
  | Wasteland
  | Tundra
  | Jungle
  | Mountain
  | Swamp
  | Forest
  | Water
  deriving Show

data TerrainFeature =
    River
  | Coast
  | Volcano
  deriving Show

data Ressource =
    Food
  | Energy
  | RawMaterial
  | Luxury
  deriving (Show,Eq,Ord)

incOwnershipDuration :: Region -> Region
incOwnershipDuration r = r {ownershipDuration = 1 + ownershipDuration r}

shiftAllegiance :: Region -> Region
shiftAllegiance r = if allegiance r /= ownerId r
  then if happiness r >= 80 && ownershipDuration r >= 5
    then r {allegiance = ownerId r}
    else r
  else r

adjustPopulationByFood :: Region -> Region
adjustPopulationByFood r = r {population = newPop, happiness = happiness r + foodDiff}
  where
    food = Map.findWithDefault 0 Food $ ressources r
    foodDiff = div ((food * 1000) - population r) 1000
    popChange = foodDiff * (div (population r) 100)
    newPop = population r + popChange

adjustPopulationByWar :: Region -> Region
adjustPopulationByWar r = r {population = newPop, happiness = happiness r - devastation r}
  where
    newPop = population r - devastation r * (div (population r) 100)

basicRegionStep :: Region -> Region
basicRegionStep = incOwnershipDuration . shiftAllegiance . adjustPopulationByFood . adjustPopulationByWar
