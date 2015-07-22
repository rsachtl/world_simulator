module Region where

data Region = Region
  { population     :: Int
  , industry       :: Int
  , infrastructure :: Int
  , devastation    :: Int
  , happiness      :: Int
  } deriving Show

data Terrain =
    Plain
  | Industrial
  | Rough
  | Desert
  | Wasteland
  | Tundra
  | Jungle
  | Mountain
  | Swamp
  | Forest
  | Water

data TerrainFeature =
    River
  | Coast
  | Volcano

data Ressource =
    Food
  | RawMaterial
  | Energy
  | Luxury

  
