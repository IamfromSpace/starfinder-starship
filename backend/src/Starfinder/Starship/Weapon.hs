{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
module Starfinder.Starship.Weapon (Class(..), Irradiation(..), Range(..), Type(..), Weapon(..), WeaponProperty(..), TurretMountClass(..), ArcMountClass(..), isTrackingWeapon, UsesMountPoints(..)) where

import Starfinder.Starship.CostsBuildPoints (CostsBuildPoints(..))
import Starfinder.Starship.DrawsPower (DrawsPower(..))
import Starfinder.Starship.Arc (Arc)
import Starfinder.Starship.Togglable (Togglable, extract)
import Data.Aeson (FromJSON, ToJSON)
import Data.Set (Set)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

data Range
    = Short
    | Medium
    | Long
    deriving (Show, Eq, Ord, Generic, Read)

instance Arbitrary Range where
  arbitrary = elements
    [ Short
    , Medium
    , Long
    ]

instance FromJSON Range
instance ToJSON Range


data Class
    = Light
    | Heavy
    | Capital
    deriving (Show, Eq, Ord)


data Type a
      -- Direct fire weapons can be a linked pair (costing two mount points)
    = DirectFire a
      -- Tracking weapons have a speed of the tracking projectile
    | Tracking Int
    deriving (Functor)


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


data Weapon a = Weapon
    { name :: Text
    , range :: Range
    , weaponClass :: Class
    , weaponType :: Type a
    , damage :: Maybe ( Int, Int )
    , powerDraw :: Int
    , buildPoints :: Int
    , specialProperties :: Set WeaponProperty
    } deriving (Functor)

instance CostsBuildPoints (Weapon Bool) where
    getBuildPoints Weapon { weaponType, buildPoints } =
      case weaponType of
        DirectFire linked -> (buildPoints * if linked then 5 else 2) `div` 2
        _ -> buildPoints

instance DrawsPower (Weapon Bool) where
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


isTrackingWeapon :: Weapon a -> Bool
isTrackingWeapon weapon =
    case weaponType weapon of
        Tracking _ ->
            True

        _ ->
            False

-- TODO: is this the right place for these to live?
-- this is a _very_ weapon related concept
class UsesMountPoints a where
  getMountPointsUsed :: a -> [Class]

instance UsesMountPoints (Weapon Bool) where
  getMountPointsUsed Weapon { weaponType, weaponClass } =
    flip replicate weaponClass $ case weaponType of
      DirectFire linked -> if linked then 2 else 1
      _ -> 1

instance UsesMountPoints a => UsesMountPoints [a] where
  getMountPointsUsed = foldMap getMountPointsUsed

instance UsesMountPoints a => UsesMountPoints (Maybe a) where
  getMountPointsUsed = fromMaybe mempty . fmap getMountPointsUsed

instance UsesMountPoints a => UsesMountPoints (Arc a) where
  getMountPointsUsed = foldMap getMountPointsUsed

instance UsesMountPoints a => UsesMountPoints (Togglable a) where
  getMountPointsUsed = getMountPointsUsed . extract
