{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Starfinder.Starship.Weapon (Class(..), Irradiation(..), Range(..), Type(..), Weapon(..), WeaponProperty(..), TurretMountClass(..), ArcMountClass(..), isTrackingWeapon, getMountPointsUsed) where

import Starfinder.Starship.CostsBuildPoints (CostsBuildPoints(..))
import Starfinder.Starship.DrawsPower (DrawsPower(..))
import Data.Set (Set)
import Data.Text (Text)

data Range
    = Short
    | Medium
    | Long
    deriving (Show, Eq, Ord)


data Class
    = Light
    | Heavy
    | Capital
    deriving (Show, Eq, Ord)


data Type
      -- Direct fire weapons can be a linked pair (costing two mount points)
    = DirectFire Bool
      -- Tracking weapons have a speed of the tracking projectile
    | Tracking Int


data Irradiation
    = High
    | MediumIrradiation
    | Low
    deriving (Show, Eq, Ord)


data WeaponProperty
    = Array
    | BroadArc
    | Emp
    | Irradiate Irradiation
    | LimitedFire Int
    | Line
    | Point Int
    | Quantum
    | Ripper
    | TractorBeam
    | Vortex
    deriving (Show, Eq, Ord)


data Weapon = Weapon
    { name :: Text
    , range :: Range
    , weaponClass :: Class
    , weaponType :: Type
    , damage :: Maybe ( Int, Int )
    , powerDraw :: Int
    , buildPoints :: Int
    , specialProperties :: Set WeaponProperty
    }

instance CostsBuildPoints Weapon where
    getBuildPoints Weapon { weaponType, buildPoints } =
      case weaponType of
        DirectFire linked -> (buildPoints * if linked then 5 else 2) `div` 2
        _ -> buildPoints

instance DrawsPower Weapon where
    getPowerDraw Weapon { weaponType, powerDraw } =
      case weaponType of
        DirectFire linked -> powerDraw * if linked then 2 else 1
        _ -> powerDraw

newtype ArcMountClass = ArcMountClass { getClass :: Class } 

instance CostsBuildPoints ArcMountClass where
    getBuildPoints x =
        case getClass (x :: ArcMountClass) of
            Light ->
                3

            Heavy ->
                7

            Capital ->
                12

newtype TurretMountClass = TurretMountClass { getClass :: Class } 

instance CostsBuildPoints TurretMountClass where
    getBuildPoints x =
        case getClass (x :: TurretMountClass) of
            Light ->
                5

            Heavy ->
                11

            Capital ->
                -- TODO: These scenarios should probably be Maybes
                100000


getMountPointsUsed :: Weapon -> [Class]
getMountPointsUsed Weapon { weaponType, weaponClass } =
  case weaponType of
    DirectFire linked -> if linked then [weaponClass, weaponClass] else [weaponClass]
    _ -> [weaponClass]


isTrackingWeapon :: Weapon -> Bool
isTrackingWeapon weapon =
    case weaponType weapon of
        Tracking _ ->
            True

        _ ->
            False
