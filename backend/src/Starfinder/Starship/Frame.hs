{-# LANGUAGE NamedFieldPuns #-}
module Starfinder.Starship.Frame (Maneuverability(..), Frame(..)) where

import Starfinder.Starship.CostsBuildPoints (CostsBuildPoints(..))
import Starfinder.Starship.Weapon (ArcMountClass, TurretMountClass)
import Starfinder.Starship.Size (Size)
import Starfinder.Starship.Arc (Arc)

data Maneuverability
    = Clumsy
    | Average
    | Good
    | Perfect


data Frame = Frame
    { name :: String
    , size :: Size
    , maneuverability :: Maneuverability
    , baseHitPoints :: Int
    , hitPointsIncrement :: Int
    , damageThreshold :: Int
    , criticalThreshold :: Int
    , arcMounts :: Arc [ArcMountClass]
    , turretMounts :: [TurretMountClass]
    , maxExpansionBays :: Int
    , minimumCrew :: Int
    , maximumCrew :: Int
    , listedBuildPoints :: Int
    }


instance CostsBuildPoints Frame where
    getBuildPoints Frame { listedBuildPoints, arcMounts, turretMounts } =
        let
            arcCosts =
                sum $ fmap (sum . fmap getBuildPoints) arcMounts

            turretCosts =
                sum $ fmap getBuildPoints turretMounts
        in
        listedBuildPoints - arcCosts - turretCosts
