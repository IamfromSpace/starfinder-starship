{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Starfinder.Starship.Build (BuildError(..), CrewQuarters(..), DriftEngine(..), Sensor(..), Shields(..), Build(..), areArcMountPointsValid, areTurretMountPointsValid, areTurretWeaponClassesValid, areWeaponClassesValidForFrame, getAllowedClasses, getArmorTargetLockBonus, getMaxHitPoints, getMaxPcuPerPowerCore, getMaxPowerCoreCount, getMountPointLimit, getPowerCoreCount, getTierFromBuildPoints, hasEnoughPowerCoresForPcu, hasSufficientPowerCoreUnits, hasSufficientPowerCoreUnitsForDriftEngine, hasTurretIfHasTurretWeapons, hasValidExpansionBayCount, hasValidPowerCoreCount, isSmallEnoughForDriftEngine, isTrue, isValidSizeForExpansionBays, isValidSpeed, maxiumumSizeForDriftEngine, minimumPowerCoreUnitsForDriftEngine, mountPointCountForGroupIsValid, validateStarship, PowerCoreUnits(..), Armor(..), DefensiveCountermeasures(..), Thrusters(..), traverseFrame, traverseWeapon, traverseShields) where

import Data.Set (member, Set, fromList)
import Data.Text (Text)
import Data.Text.Arbitrary
import Data.Aeson (FromJSON, ToJSON, toJSON, parseJSON)
import GHC.Generics (Generic)
import Starfinder.Starship.Arc (Arc)
import Starfinder.Starship.Computer (Computer)
import Starfinder.Starship.DefenseLevel (DefenseLevel(..))
import Starfinder.Starship.ExpansionBay (ExpansionBay(..), isValidSize, getExpansionBaysUsed)
import Starfinder.Starship.Frame (Frame(..))
import Starfinder.Starship.Size (Size(..), Sized(..), topSpeed)
import Starfinder.Starship.Togglable (Togglable, extract)
import Starfinder.Starship.Weapon (Weapon(..), getMountPointsUsed, isTrackingWeapon, TurretMountClass(..), ArcMountClass(..))
import qualified Starfinder.Starship.Weapon as Weapon
import Starfinder.Starship.CostsBuildPoints (CostsBuildPoints(..))
import Starfinder.Starship.DrawsPower (DrawsPower(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)


newtype Thrusters = Thrusters { getSpeed :: Int } deriving (Show, Eq)

instance Arbitrary Thrusters where
  arbitrary = Thrusters <$> arbitrary

instance FromJSON Thrusters where
  parseJSON v = Thrusters <$> parseJSON v

instance ToJSON Thrusters where
  toJSON = toJSON . getSpeed

instance DrawsPower (Sized Thrusters) where
    getPowerDraw (Sized size t) =
        let
            speed = getSpeed t
        in case size of
            Tiny ->
                (speed * 5 + 10) `div` 2

            Small ->
                speed * 5

            Medium ->
                speed * 5 + 20

            Large ->
                speed * 10 + 20

            Huge ->
                speed * 20

            Gargantuan ->
                speed * 30

            Colossal ->
                speed * 50


instance CostsBuildPoints (Sized Thrusters) where
    getBuildPoints (Sized size t) =
        let
            speed = getSpeed t
        in case size of
            Large ->
                speed

            Huge ->
                speed

            Gargantuan ->
                speed * 2

            Colossal ->
                speed * 2

            _ ->
                speed `div` 2

newtype Armor = Armor { getDefenseLevelA :: DefenseLevel } deriving (Show, Eq)

instance Arbitrary Armor where
  arbitrary = Armor <$> arbitrary

instance FromJSON Armor where
  parseJSON v = Armor <$> parseJSON v

instance ToJSON Armor where
  toJSON = toJSON . getDefenseLevelA

instance CostsBuildPoints (Sized Armor) where
    getBuildPoints (Sized size a) =
        let
            costMultiplier =
                case getDefenseLevelA a of
                    Mk1 ->
                        1

                    Mk2 ->
                        2

                    Mk3 ->
                        3

                    Mk4 ->
                        5

                    Mk5 ->
                        7

                    Mk6 ->
                        9

                    Mk7 ->
                        12

                    Mk8 ->
                        15

                    Mk9 ->
                        18

                    Mk10 ->
                        21

                    Mk11 ->
                        25

                    Mk12 ->
                        30

                    Mk13 ->
                        35

                    Mk14 ->
                        40

                    Mk15 ->
                        45
        in
        fromEnum size * costMultiplier


getArmorTargetLockBonus :: Armor -> Int
getArmorTargetLockBonus armor =
    case getDefenseLevelA armor of
        Mk1 ->
            0

        Mk2 ->
            0

        Mk3 ->
            0

        Mk4 ->
            0

        Mk5 ->
            -1

        Mk6 ->
            -1

        Mk7 ->
            -1

        Mk8 ->
            -1

        Mk9 ->
            -2

        Mk10 ->
            -2

        Mk11 ->
            -2

        Mk12 ->
            -3

        Mk13 ->
            -3

        Mk14 ->
            -3

        Mk15 ->
            -4


data CrewQuarters
    = Common
    | GoodQuarters
    | Luxurious
    deriving (Generic, Show, Read, Eq)

instance Arbitrary CrewQuarters where
  arbitrary = elements
    [ Common
    , GoodQuarters
    , Luxurious
    ]

instance FromJSON CrewQuarters
instance ToJSON CrewQuarters


instance CostsBuildPoints CrewQuarters where
    getBuildPoints crewQuarters =
        case crewQuarters of
            Common ->
                0

            GoodQuarters ->
                2

            Luxurious ->
                5

newtype DefensiveCountermeasures = DefensiveCountermeasures { getDefenseLevel :: DefenseLevel } deriving (Show, Eq)

instance Arbitrary DefensiveCountermeasures where
  arbitrary = DefensiveCountermeasures <$> arbitrary

instance FromJSON DefensiveCountermeasures where
  parseJSON v = DefensiveCountermeasures <$> parseJSON v

instance ToJSON DefensiveCountermeasures where
  toJSON = toJSON . getDefenseLevel

instance DrawsPower DefensiveCountermeasures where
    getPowerDraw =
        (`div` 2) . getBuildPoints


instance CostsBuildPoints DefensiveCountermeasures where
    getBuildPoints dcm =
        case getDefenseLevel dcm of
            Mk1 ->
                2

            Mk2 ->
                3

            Mk3 ->
                4

            Mk4 ->
                6

            Mk5 ->
                8

            Mk6 ->
                11

            Mk7 ->
                14

            Mk8 ->
                18

            Mk9 ->
                22

            Mk10 ->
                27

            Mk11 ->
                33

            Mk12 ->
                40

            Mk13 ->
                50

            Mk14 ->
                65

            Mk15 ->
                90

newtype PowerCoreUnits = PowerCoreUnits { getUnits :: Int } deriving (Show, Eq)

instance Arbitrary PowerCoreUnits where
  arbitrary = PowerCoreUnits <$> arbitrary

instance FromJSON PowerCoreUnits where
  parseJSON v = PowerCoreUnits <$> parseJSON v

instance ToJSON PowerCoreUnits where
  toJSON = toJSON . getUnits

instance CostsBuildPoints PowerCoreUnits where
    getBuildPoints pcu =
        -- Totally ignoring that like 2 numbers are incorrect here, because it's stupid
        (((getUnits pcu) - 1) `div` 10) + 1


data DriftEngine
    = Basic
    | Booster
    | Major
    | Superior
    | Ultra
    deriving (Generic, Show, Read, Eq)

instance Arbitrary DriftEngine where
  arbitrary = elements
    [ Basic
    , Booster
    , Major
    , Superior
    , Ultra
    ]

instance FromJSON DriftEngine
instance ToJSON DriftEngine


instance CostsBuildPoints (Sized DriftEngine) where
    getBuildPoints (Sized size driftEngine) =
        fromEnum size
            * (case driftEngine of
                Basic ->
                    2

                Booster ->
                    5

                Major ->
                    10

                Superior ->
                    15

                Ultra ->
                    20
              )


data Sensor = Sensor
    { range :: Weapon.Range
    , bonus :: Int
    } deriving (Generic, Show, Eq)

instance Arbitrary Sensor where
  arbitrary = Sensor <$> arbitrary <*> arbitrary

instance FromJSON Sensor
instance ToJSON Sensor


instance CostsBuildPoints Sensor where
    getBuildPoints Sensor { range, bonus } =
        case range of
            Weapon.Short ->
                bonus `div` 2 + 2

            Weapon.Medium ->
                bonus + 3

            Weapon.Long ->
                bonus * 2 + 6


data Shields = Shields
    { name :: Text
    , shieldPoints :: Int
    , regenPerMinute :: Int
    , powerDraw :: Int
    , buildPoints :: Int
    }

instance CostsBuildPoints Shields where
    getBuildPoints Shields { buildPoints } = buildPoints

instance DrawsPower Shields where
    getPowerDraw Shields { powerDraw } = powerDraw


--TODO: Security Systems


-- TODO: Consider making names case insensitive
data Build frame weapon shields = Build
    { name :: Text
    , frame :: frame
    , powerCoreUnits :: PowerCoreUnits
    , thrusters :: Togglable Thrusters
    , armor :: Maybe Armor
    , computer :: Togglable Computer
    , crewQuarters :: CrewQuarters
    , defensiveCountermeasures :: Maybe (Togglable DefensiveCountermeasures)
    , driftEngine :: Maybe DriftEngine
    , expansionBays :: [Togglable ExpansionBay]
    , sensors :: Sensor
    , arcWeapons :: Arc [Togglable weapon]
    , turretWeapons :: [Togglable weapon]
    , shields :: Togglable shields
    } deriving (Generic, Show, Eq)

-- TODO: Newtype wrapper to enable these
traverseFrame :: Applicative f => (a -> f b) -> Build a x y -> f (Build b x y)
traverseFrame fn (Build a frame c d e f g h i j k l m n) =
  (\b -> Build a b c d e f g h i j k l m n) <$> fn frame

traverseWeapon :: Applicative f => (a -> f b) -> Build x a y -> f (Build x b y)
traverseWeapon fn (Build a b c d e f g h i j k arcWeapons turretWeapons n) =
  Build a b c d e f g h i j k <$>
    traverse (traverse (traverse fn)) arcWeapons <*>
    traverse (traverse fn) turretWeapons <*>
    pure n

traverseShields :: Applicative f => (a -> f b) -> Build x y a -> f (Build x y b)
traverseShields fn (Build a b c d e f g h i j k l m shields) =
  Build a b c d e f g h i j k l m <$>
    traverse fn shields

instance (Arbitrary a,Arbitrary b,Arbitrary c) => Arbitrary (Build a b c) where
  arbitrary = Build <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (Build a b c)
instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (Build a b c)


instance DrawsPower (Build Frame (Weapon Bool) Shields) where
    getPowerDraw Build { frame = Frame { size }, thrusters, computer, defensiveCountermeasures, expansionBays, arcWeapons, turretWeapons, shields } =
      getPowerDraw (fmap (Sized size) thrusters)
        + getPowerDraw computer
        + getPowerDraw defensiveCountermeasures
        + getPowerDraw expansionBays
        + getPowerDraw arcWeapons
        + getPowerDraw turretWeapons
        + getPowerDraw shields


instance CostsBuildPoints (Build Frame (Weapon Bool) Shields) where
    getBuildPoints Build { frame = Frame { size }, powerCoreUnits, thrusters, armor, computer, crewQuarters, defensiveCountermeasures, driftEngine, expansionBays, sensors, arcWeapons, turretWeapons, shields } =
      getBuildPoints powerCoreUnits
        + getBuildPoints (fmap (Sized size) thrusters)
        + getBuildPoints (fmap (Sized size) armor)
        + getBuildPoints computer
        + getBuildPoints crewQuarters
        + getBuildPoints defensiveCountermeasures
        + getBuildPoints (fmap (Sized size) driftEngine)
        + getBuildPoints expansionBays
        + getBuildPoints sensors
        + getBuildPoints arcWeapons
        + getBuildPoints turretWeapons
        + getBuildPoints shields
        -- Mount Points are a bit of an oddball because we need more context
        -- TODO: Could create a UsesMountPoints typeclass for Arc, List, Togglable, Weapon...
        + (getBuildPoints
             $ fmap ArcMountClass
             $ getMountPointsUsed arcWeapons)
        + (getBuildPoints
             $ fmap TurretMountClass
             $ getMountPointsUsed turretWeapons)


getTierFromBuildPoints :: Int -> Float
getTierFromBuildPoints bp =
    if bp < 25 then
        1 / 4

    else if bp < 30 then
        1 / 3

    else if bp < 40 then
        1 / 2

    else if bp < 55 then
        1

    else if bp < 75 then
        2

    else if bp < 95 then
        3

    else if bp < 115 then
        4

    else if bp < 135 then
        5

    else if bp < 155 then
        6

    else if bp < 180 then
        7

    else if bp < 205 then
        8

    else if bp < 230 then
        9

    else if bp < 270 then
        10

    else if bp < 310 then
        11

    else if bp < 350 then
        12

    else if bp < 400 then
        13

    else if bp < 450 then
        14

    else if bp < 500 then
        15

    else if bp < 600 then
        16

    else if bp < 700 then
        17

    else if bp < 800 then
        18

    else if bp < 900 then
        19

    else
        20


getMaxHitPoints :: (Build Frame (Weapon Bool) Shields) -> Int
getMaxHitPoints build@Build { frame = Frame { baseHitPoints, hitPointsIncrement } } =
    let
        increases =
            (round $ getTierFromBuildPoints $ getBuildPoints build) `div` 4
    in
    baseHitPoints + hitPointsIncrement * increases



-- Validate Arc Mounted/Turret Mounted Weapon Count
-- Validate Turret Mounted Class (no Capital weapons)
-- Validate Weapon Mounted Class vs Size (Heavy Weapons on < Medium, etc)
-- Validate that turret-less frames have no turret mounted weapons
-- Validate Power Core Count agains max per size + allowed expansions
-- Validate ExpansionBay Count
-- Validate PCU Total against max per size + expansions
-- Validate total power draw
-- Validate Drift Engine Rating (against miminum PCU)


getMountPointLimit :: Size -> Int
getMountPointLimit size =
    case size of
        Tiny ->
            2

        Small ->
            2

        Medium ->
            3

        Large ->
            3

        _ ->
            4


getAllowedClasses :: Size -> Set Weapon.Class
getAllowedClasses size =
    case size of
        Tiny ->
            fromList [ Weapon.Light ]

        Small ->
            fromList [ Weapon.Light ]

        Medium ->
            fromList [ Weapon.Light, Weapon.Heavy ]

        Large ->
            fromList [ Weapon.Light, Weapon.Heavy ]

        _ ->
            fromList [ Weapon.Light, Weapon.Heavy, Weapon.Capital ]


getMaxPowerCoreCount :: Size -> Int
getMaxPowerCoreCount size =
    case size of
        Colossal ->
            4

        Gargantuan ->
            3

        Medium ->
            2

        Large ->
            2

        _ ->
            1


getMaxPcuPerPowerCore :: Size -> Int
getMaxPcuPerPowerCore size =
    case size of
        Tiny ->
            200

        Small ->
            300

        Medium ->
            300

        Large ->
            400

        _ ->
            500


minimumPowerCoreUnitsForDriftEngine :: DriftEngine -> Int
minimumPowerCoreUnitsForDriftEngine driftEngine =
    case driftEngine of
        Basic ->
            75

        Booster ->
            100

        Major ->
            150

        Superior ->
            175

        Ultra ->
            200


maxiumumSizeForDriftEngine :: DriftEngine -> Size
maxiumumSizeForDriftEngine driftEngine =
    case driftEngine of
        Basic ->
            Colossal

        Booster ->
            Huge

        Major ->
            Large

        Superior ->
            Large

        Ultra ->
            Medium


mountPointCountForGroupIsValid :: Size -> [Togglable (Weapon Bool)] -> Bool
mountPointCountForGroupIsValid size group =
    length (concatMap (getMountPointsUsed . extract) group) <= getMountPointLimit size


areArcMountPointsValid :: Build Frame (Weapon Bool) a -> Bool
areArcMountPointsValid Build { arcWeapons, frame = Frame { size } } =
    all (mountPointCountForGroupIsValid size) arcWeapons


areTurretMountPointsValid :: Build Frame (Weapon Bool) a -> Bool
areTurretMountPointsValid Build { turretWeapons, frame = Frame { size } } =
    mountPointCountForGroupIsValid size turretWeapons


areWeaponClassesValidForFrame :: Build Frame (Weapon Bool) a -> Bool
areWeaponClassesValidForFrame Build { arcWeapons, turretWeapons, frame = Frame { size } } =
    all
        (\a -> member (weaponClass (extract a)) (getAllowedClasses size))
        (concat arcWeapons <> turretWeapons)


areTurretWeaponClassesValid :: Build Frame (Weapon Bool) a -> Bool
areTurretWeaponClassesValid Build { turretWeapons, frame } =
    all
        ((/=) Weapon.Capital . weaponClass . extract)
        turretWeapons


hasTurretIfHasTurretWeapons :: Build Frame (Weapon Bool) a -> Bool
hasTurretIfHasTurretWeapons Build { turretWeapons, frame = Frame { turretMounts } } =
    length turretWeapons == 0 || length turretMounts > 0



getPowerCoreCount :: Build a b c -> Int
getPowerCoreCount Build { expansionBays } =
    let
        isPowerCoreHousing =
            (==) PowerCoreHousing . extract
    in
    length (filter isPowerCoreHousing expansionBays) + 1


hasEnoughPowerCoresForPcu :: Build Frame a b -> Bool
hasEnoughPowerCoresForPcu build@Build { powerCoreUnits, frame = Frame { size } } =
    getUnits powerCoreUnits <= getPowerCoreCount build * getMaxPcuPerPowerCore size


hasValidPowerCoreCount :: Build Frame a b -> Bool
hasValidPowerCoreCount build@Build { frame = Frame { size } } =
    getPowerCoreCount build <= getMaxPowerCoreCount size


hasValidExpansionBayCount :: Build Frame a b -> Bool
hasValidExpansionBayCount Build { expansionBays, frame } =
    let
        baysUsed = sum
            $ fmap (getExpansionBaysUsed . extract)
            $ expansionBays
    in
    baysUsed <= maxExpansionBays frame


isValidSizeForExpansionBays :: Build Frame a b -> Bool
isValidSizeForExpansionBays Build { frame, expansionBays } =
    all (isValidSize (size frame) . extract) expansionBays


hasSufficientPowerCoreUnits :: Build Frame (Weapon Bool) Shields -> Bool
hasSufficientPowerCoreUnits build =
    getUnits (powerCoreUnits build) >= getPowerDraw build


hasSufficientPowerCoreUnitsForDriftEngine :: Build a b c -> Bool
hasSufficientPowerCoreUnitsForDriftEngine Build { driftEngine, powerCoreUnits } =
    case driftEngine of
        Just driftEngine ->
            minimumPowerCoreUnitsForDriftEngine driftEngine <= getUnits powerCoreUnits

        Nothing ->
            True


isSmallEnoughForDriftEngine :: Build Frame a b -> Bool
isSmallEnoughForDriftEngine Build { driftEngine, frame = Frame { size } } =
    case driftEngine of
        Just dE ->
            fromEnum size
                <= fromEnum (maxiumumSizeForDriftEngine dE)

        Nothing ->
            True


isValidSpeed :: Build Frame a b -> Bool
isValidSpeed Build { thrusters, frame = Frame { size } } =
  let speed = getSpeed $ extract thrusters
  in speed <= topSpeed size && speed > 0


data BuildError
    = TooManyWeaponMountsOnArc
    | TooManyWeaponMountsOnTurret
    | InvalidWeaponClassOnFrame
    | IllegalCapitalWeaponOnTurret
    | IllegalTurretMountsOnTurretlessFrame
    | PcuRequiresRequiresAdditionalPowerCore
    | TooManyPowerCores
    | TooManyExpansionBays
    | InvalidExpansionBayOnFrame
    | NotEnoughPowerForActiveSystems
    | PowerCoreTooSmallForDriftEngines
    | ShipToLargeForDriftEngine
    | SpeedTooFastForSizeOrLessThan1


isTrue :: (a -> Bool) -> b -> ( [b], a ) -> ( [b], a )
isTrue fn err ( errs, value ) =
    if fn value then
        ( errs, value )

    else
        ( err : errs, value )


validateStarship :: Build Frame (Weapon Bool) Shields -> [BuildError]
validateStarship =
    fst
        . isTrue areArcMountPointsValid TooManyWeaponMountsOnArc
        . isTrue areTurretMountPointsValid TooManyWeaponMountsOnTurret
        . isTrue areWeaponClassesValidForFrame InvalidWeaponClassOnFrame
        . isTrue areTurretWeaponClassesValid IllegalCapitalWeaponOnTurret
        . isTrue hasTurretIfHasTurretWeapons IllegalTurretMountsOnTurretlessFrame
        . isTrue hasEnoughPowerCoresForPcu PcuRequiresRequiresAdditionalPowerCore
        . isTrue hasValidPowerCoreCount TooManyPowerCores
        . isTrue hasValidExpansionBayCount TooManyExpansionBays
        . isTrue isValidSizeForExpansionBays InvalidExpansionBayOnFrame
        . isTrue hasSufficientPowerCoreUnits NotEnoughPowerForActiveSystems
        . isTrue hasSufficientPowerCoreUnitsForDriftEngine PowerCoreTooSmallForDriftEngines
        . isTrue isSmallEnoughForDriftEngine ShipToLargeForDriftEngine
        . isTrue isValidSpeed SpeedTooFastForSizeOrLessThan1
        . (\b -> ( [], b ))
