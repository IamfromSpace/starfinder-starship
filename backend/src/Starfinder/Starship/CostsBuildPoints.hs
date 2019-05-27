module Starfinder.Starship.CostsBuildPoints (CostsBuildPoints(..)) where

class CostsBuildPoints a where
  getBuildPoints :: a -> Int


instance CostsBuildPoints a => CostsBuildPoints (Maybe a) where
    getBuildPoints (Just a) = getBuildPoints a
    getBuildPoints Nothing = 0


instance CostsBuildPoints a => CostsBuildPoints  [a] where
    getBuildPoints = sum . fmap getBuildPoints
