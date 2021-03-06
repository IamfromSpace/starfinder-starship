module Starship exposing (BuildError(..), CrewQuarters(..), DriftEngine(..), Sensor, Shields, Starship, areArcMountPointsValid, areTurretMountPointsValid, areTurretWeaponClassesValid, areWeaponClassesValidForFrame, buildErrorToString, crewQuartersToString, driftEngineToString, getAcModifier, getAllOnlineWeapons, getAllWeapons, getAllowedClasses, getArmorBuildPoints, getArmorTargetLockBonus, getCrewQuartersBuildPoints, getDefensiveCountermeasuresBuildPoints, getDefensiveCountermeasuresPowerDraw, getDistanceBetweenTurnsModifier, getDriftEngineBuildPoints, getMaxHitPoints, getMaxPcuPerPowerCore, getMaxPowerCoreCount, getMountPointLimit, getMountPointsBuiltPoints, getMountPointsUsed, getPilotBonus, getPowerCoreCount, getPowerCoreUnitsBuildPoints, getSensorBuildPoints, getSizeCategory, getSpeedModifier, getStarshipBuildPoints, getStarshipPowerDraw, getThrusterBuildPoints, getThrusterPowerDraw, getTierFromBuildPoints, getTlModifier, getTogglablePowerDraw, getWeaponsBuildPoints, getWeaponsPowerDraw, hasEnoughPowerCoresForPcu, hasNoTrackingWeaponLinks, hasSufficientPowerCoreUnits, hasSufficientPowerCoreUnitsForDriftEngine, hasTurretIfHasTurretWeapons, hasValidExpansionBayCount, hasValidPowerCoreCount, isSmallEnoughForDriftEngine, isTrue, isValidSizeForExpansionBays, isValidSpeed, maxiumumSizeForDriftEngine, minimumPowerCoreUnitsForDriftEngine, mountPointCountForGroupIsValid, validateStarship)

import Arc exposing (Arc)
import Computer exposing (Computer)
import DefenseLevel exposing (..)
import ExpansionBay exposing (ExpansionBay)
import Frame exposing (Frame)
import Link exposing (..)
import LinkAndTogglable as LT exposing (LinkAndTogglable)
import Size exposing (..)
import Switch exposing (..)
import Togglable exposing (..)
import Weapon exposing (Weapon)


getThrusterPowerDraw : Starship -> Int
getThrusterPowerDraw { frame, thrusters } =
    let
        speed =
            extract thrusters
    in
    case meta thrusters of
        Off ->
            0

        On ->
            case frame.size of
                Tiny ->
                    round (toFloat speed * 2.5 + 5)

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


getThrusterBuildPoints : Starship -> Int
getThrusterBuildPoints { frame, thrusters } =
    let
        speed =
            extract thrusters
    in
    case frame.size of
        Large ->
            speed

        Huge ->
            speed

        Gargantuan ->
            speed * 2

        Colossal ->
            speed * 2

        _ ->
            round (toFloat speed * 0.5)


getSizeCategory : Size -> Int
getSizeCategory size =
    case size of
        Tiny ->
            1

        Small ->
            2

        Medium ->
            3

        Large ->
            4

        Huge ->
            5

        Gargantuan ->
            6

        Colossal ->
            7


getArmorBuildPoints : Size -> DefenseLevel -> Int
getArmorBuildPoints size defenseLevel =
    let
        costMultiplier =
            case defenseLevel of
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
    getSizeCategory size * costMultiplier


getArmorTargetLockBonus : DefenseLevel -> Int
getArmorTargetLockBonus armor =
    case armor of
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


type CrewQuarters
    = Common
    | GoodQuarters
    | Luxurious


crewQuartersToString : CrewQuarters -> String
crewQuartersToString crewQuarters =
    case crewQuarters of
        Common ->
            "Common"

        GoodQuarters ->
            "GoodQuarters"

        Luxurious ->
            "Luxurious"


getCrewQuartersBuildPoints : CrewQuarters -> Int
getCrewQuartersBuildPoints crewQuarters =
    case crewQuarters of
        Common ->
            0

        GoodQuarters ->
            2

        Luxurious ->
            5


getDefensiveCountermeasuresPowerDraw : DefenseLevel -> Int
getDefensiveCountermeasuresPowerDraw =
    getDefensiveCountermeasuresBuildPoints >> (\x -> x // 2)


getDefensiveCountermeasuresBuildPoints : DefenseLevel -> Int
getDefensiveCountermeasuresBuildPoints defensiveCountermeasures =
    case defensiveCountermeasures of
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


getPowerCoreUnitsBuildPoints : Int -> Int
getPowerCoreUnitsBuildPoints pcu =
    -- Totally ignoring that like 2 numbers are incorrect here, because it's stupid
    ((pcu - 1) // 10) + 1


type DriftEngine
    = Basic
    | Booster
    | Major
    | Superior
    | Ultra


driftEngineToString : DriftEngine -> String
driftEngineToString driftEngine =
    case driftEngine of
        Basic ->
            "Basic"

        Booster ->
            "Booster"

        Major ->
            "Major"

        Superior ->
            "Superior"

        Ultra ->
            "Ultra"


getDriftEngineBuildPoints : Size -> DriftEngine -> Int
getDriftEngineBuildPoints size driftEngine =
    getSizeCategory size
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


type alias Sensor =
    { range : Weapon.Range
    , bonus : Int
    }


getSensorBuildPoints : Sensor -> Int
getSensorBuildPoints { range, bonus } =
    case range of
        Weapon.Short ->
            bonus // 2 + 2

        Weapon.Medium ->
            bonus + 3

        Weapon.Long ->
            bonus * 2 + 6


getAllWeapons : Starship -> List (LinkAndTogglable Weapon)
getAllWeapons { arcWeapons, turretWeapons } =
    Arc.concat arcWeapons ++ turretWeapons


getAllOnlineWeapons : Starship -> List (LinkAndTogglable Weapon)
getAllOnlineWeapons =
    getAllWeapons
        >> List.filter (LT.meta >> .switch >> (==) On)


getWeaponsPowerDraw : Starship -> Int
getWeaponsPowerDraw =
    let
        getDraw lt =
            .powerDraw (LT.extract lt)
                * (if .link (LT.meta lt) == Linked then
                    2

                   else
                    1
                  )
    in
    getAllOnlineWeapons >> List.map getDraw >> List.sum


getWeaponsBuildPoints : Starship -> Int
getWeaponsBuildPoints =
    let
        getPoints lt =
            round <|
                toFloat (.buildPoints (LT.extract lt))
                    * (if .link (LT.meta lt) == Linked then
                        2.5

                       else
                        1
                      )
    in
    getAllWeapons >> List.map getPoints >> List.sum



-- TODO: Array weapons use 2 mount points


getMountPointsUsed : LinkAndTogglable Weapon -> Int
getMountPointsUsed weapon =
    if .link (LT.meta weapon) == Linked then
        2

    else
        1


getMountPointsBuiltPoints : Starship -> Int
getMountPointsBuiltPoints ship =
    let
        getLinkableCost getBuildPoints weapon =
            (LT.extract >> .weaponClass >> getBuildPoints) weapon
                * getMountPointsUsed weapon

        arcCost =
            List.map
                (getLinkableCost Weapon.getArcMountPointBuiltPoints)
                (Arc.concat ship.arcWeapons)

        turretCost =
            List.map
                (getLinkableCost Weapon.getTurretMountPointBuiltPoints)
                ship.turretWeapons
    in
    List.sum (arcCost ++ turretCost)


type alias Shields =
    { name : String
    , shieldPoints : Int
    , regenPerMinute : Int
    , powerDraw : Int
    , buildPoints : Int
    }



--TODO: Security Systems


type alias Starship =
    { name : String
    , frame : Frame
    , powerCoreUnits : Int
    , thrusters : Togglable Int
    , armor : Maybe DefenseLevel
    , computer : Togglable Computer
    , crewQuarters : CrewQuarters
    , defensiveCountermeasures : Maybe (Togglable DefenseLevel)
    , driftEngine : Maybe DriftEngine
    , expansionBays : List (Togglable ExpansionBay)
    , sensors : Sensor
    , arcWeapons : Arc (List (LinkAndTogglable Weapon))
    , turretWeapons : List (LinkAndTogglable Weapon)
    , shields : Togglable Shields
    }


getPilotBonusThrusters_ : Togglable Int -> Int
getPilotBonusThrusters_ thrusters =
    let
        effectiveSpeed =
            if meta thrusters == Switch.Off then
                0

            else
                extract thrusters
    in
    if effectiveSpeed < 4 then
        2

    else if effectiveSpeed < 6 then
        1

    else if effectiveSpeed < 10 then
        0

    else if effectiveSpeed < 12 then
        1

    else
        2


getPilotBonus : Starship -> Int
getPilotBonus { thrusters, frame } =
    getPilotBonusThrusters_ thrusters + Frame.getPilotBonus frame.maneuverability


getTogglablePowerDraw : (a -> Int) -> Togglable a -> Int
getTogglablePowerDraw powerDrawFn togglable =
    case meta togglable of
        On ->
            powerDrawFn (extract togglable)

        Off ->
            0


getStarshipPowerDraw : Starship -> Int
getStarshipPowerDraw ship =
    getThrusterPowerDraw ship
        + getTogglablePowerDraw Computer.getPowerDraw ship.computer
        + (ship.defensiveCountermeasures
            |> Maybe.map (getTogglablePowerDraw getDefensiveCountermeasuresPowerDraw)
            |> Maybe.withDefault 0
          )
        + List.sum (List.map (getTogglablePowerDraw ExpansionBay.getPowerDraw) ship.expansionBays)
        + getWeaponsPowerDraw ship
        + getTogglablePowerDraw .powerDraw ship.shields


getStarshipBuildPoints : Starship -> Int
getStarshipBuildPoints ship =
    let
        computer =
            extract ship.computer

        shields =
            extract ship.shields
    in
    Frame.getBuildPoints ship.frame
        + getPowerCoreUnitsBuildPoints ship.powerCoreUnits
        + getThrusterBuildPoints ship
        + (ship.armor
            |> Maybe.map (getArmorBuildPoints ship.frame.size)
            |> Maybe.withDefault 0
          )
        + Computer.getBuildPoints computer
        + getCrewQuartersBuildPoints ship.crewQuarters
        + (ship.defensiveCountermeasures
            |> Maybe.map
                (extract >> getDefensiveCountermeasuresBuildPoints)
            |> Maybe.withDefault 0
          )
        + (ship.driftEngine
            |> Maybe.map (getDriftEngineBuildPoints ship.frame.size)
            |> Maybe.withDefault 0
          )
        + List.sum (List.map (extract >> ExpansionBay.getBuildPoints) ship.expansionBays)
        + getSensorBuildPoints ship.sensors
        + getWeaponsBuildPoints ship
        + getMountPointsBuiltPoints ship
        + shields.buildPoints


getTierFromBuildPoints : Int -> Float
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


getMaxHitPoints : Starship -> Int
getMaxHitPoints ship =
    let
        increases =
            (ship |> getStarshipBuildPoints |> getTierFromBuildPoints |> round) // 4
    in
    ship.frame.baseHitPoints + ship.frame.hitPointsIncrement * increases



-- Validate Arc Mounted/Turret Mounted Weapon Count
-- Validate Turret Mounted Class (no Capital weapons)
-- Validate Weapon Mounted Class vs Size (Heavy Weapons on < Medium, etc)
-- Validate that turret-less frames have no turret mounted weapons
-- Validate Power Core Count agains max per size + allowed expansions
-- Validate ExpansionBay Count
-- Validate PCU Total against max per size + expansions
-- Validate total power draw
-- Validate Drift Engine Rating (against miminum PCU)


getMountPointLimit : Size -> Int
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


getAllowedClasses : Size -> List Weapon.Class
getAllowedClasses size =
    case size of
        Tiny ->
            [ Weapon.Light ]

        Small ->
            [ Weapon.Light ]

        Medium ->
            [ Weapon.Light, Weapon.Heavy ]

        Large ->
            [ Weapon.Light, Weapon.Heavy ]

        _ ->
            [ Weapon.Light, Weapon.Heavy, Weapon.Capital ]


getMaxPowerCoreCount : Size -> Int
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


getMaxPcuPerPowerCore : Size -> Int
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


minimumPowerCoreUnitsForDriftEngine : DriftEngine -> Int
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


maxiumumSizeForDriftEngine : DriftEngine -> Size
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


mountPointCountForGroupIsValid : Size -> List (LinkAndTogglable Weapon) -> Bool
mountPointCountForGroupIsValid size group =
    List.sum (List.map getMountPointsUsed group) <= getMountPointLimit size


areArcMountPointsValid : Starship -> Bool
areArcMountPointsValid { arcWeapons, frame } =
    Arc.all <| Arc.map (mountPointCountForGroupIsValid frame.size) arcWeapons


areTurretMountPointsValid : Starship -> Bool
areTurretMountPointsValid { turretWeapons, frame } =
    mountPointCountForGroupIsValid frame.size turretWeapons


areWeaponClassesValidForFrame : Starship -> Bool
areWeaponClassesValidForFrame { arcWeapons, turretWeapons, frame } =
    List.all
        (LT.extract
            >> .weaponClass
            >> (\a -> List.member a (getAllowedClasses frame.size))
        )
        (Arc.concat arcWeapons ++ turretWeapons)


areTurretWeaponClassesValid : Starship -> Bool
areTurretWeaponClassesValid { turretWeapons, frame } =
    List.all
        (LT.extract >> .weaponClass >> (/=) Weapon.Capital)
        turretWeapons


hasTurretIfHasTurretWeapons : Starship -> Bool
hasTurretIfHasTurretWeapons { turretWeapons, frame } =
    List.length turretWeapons == 0 || List.length frame.turretMounts > 0


hasNoTrackingWeaponLinks : Starship -> Bool
hasNoTrackingWeaponLinks { arcWeapons, turretWeapons } =
    List.all
        (\ltWeapon ->
            not (Weapon.isTrackingWeapon (LT.extract ltWeapon))
                || not (.link (LT.meta ltWeapon) == Linked)
        )
        (Arc.concat arcWeapons ++ turretWeapons)


getPowerCoreCount : Starship -> Int
getPowerCoreCount { expansionBays } =
    let
        isPowerCoreHousing =
            extract >> (==) ExpansionBay.PowerCoreHousing
    in
    List.length (List.filter isPowerCoreHousing expansionBays) + 1


hasEnoughPowerCoresForPcu : Starship -> Bool
hasEnoughPowerCoresForPcu ship =
    ship.powerCoreUnits <= getPowerCoreCount ship * getMaxPcuPerPowerCore ship.frame.size


hasValidPowerCoreCount : Starship -> Bool
hasValidPowerCoreCount ship =
    getPowerCoreCount ship <= getMaxPowerCoreCount ship.frame.size


hasValidExpansionBayCount : Starship -> Bool
hasValidExpansionBayCount { expansionBays, frame } =
    let
        baysUsed =
            expansionBays
                |> List.map (extract >> ExpansionBay.getExpansionBaysUsed)
                |> List.sum
    in
    baysUsed <= frame.expansionBays


isValidSizeForExpansionBays : Starship -> Bool
isValidSizeForExpansionBays { frame, expansionBays } =
    List.all (extract >> ExpansionBay.isValidSize frame.size) expansionBays


hasSufficientPowerCoreUnits : Starship -> Bool
hasSufficientPowerCoreUnits ship =
    ship.powerCoreUnits >= getStarshipPowerDraw ship


hasSufficientPowerCoreUnitsForDriftEngine : Starship -> Bool
hasSufficientPowerCoreUnitsForDriftEngine ship =
    case ship.driftEngine of
        Just driftEngine ->
            minimumPowerCoreUnitsForDriftEngine driftEngine <= ship.powerCoreUnits

        Nothing ->
            True


isSmallEnoughForDriftEngine : Starship -> Bool
isSmallEnoughForDriftEngine { driftEngine, frame } =
    case driftEngine of
        Just dE ->
            getSizeCategory frame.size
                <= getSizeCategory (maxiumumSizeForDriftEngine dE)

        Nothing ->
            True


isValidSpeed : Starship -> Bool
isValidSpeed { frame, thrusters } =
    let
        speed =
            extract thrusters
    in
    speed <= Size.topSpeed frame.size && speed > 0


type BuildError
    = TooManyWeaponMountsOnArc
    | TooManyWeaponMountsOnTurret
    | InvalidWeaponClassOnFrame
    | IllegalCapitalWeaponOnTurret
    | IllegalLinkedTrackingWeapon
    | PcuRequiresRequiresAdditionalPowerCore
    | TooManyPowerCores
    | TooManyExpansionBays
    | InvalidExpansionBayOnFrame
    | NotEnoughPowerForActiveSystems
    | PowerCoreTooSmallForDriftEngines
    | ShipToLargeForDriftEngine
    | SpeedTooFastForSizeOrLessThan1


buildErrorToString : BuildError -> String
buildErrorToString buildError =
    case buildError of
        TooManyWeaponMountsOnArc ->
            "TooManyWeaponMountsOnArc"

        TooManyWeaponMountsOnTurret ->
            "TooManyWeaponMountsOnTurret"

        InvalidWeaponClassOnFrame ->
            "InvalidWeaponClassOnFrame"

        IllegalCapitalWeaponOnTurret ->
            "IllegalCapitalWeaponOnTurret"

        IllegalLinkedTrackingWeapon ->
            "IllegalLinkedTrackingWeapon"

        PcuRequiresRequiresAdditionalPowerCore ->
            "PcuRequiresRequiresAdditionalPowerCore"

        TooManyPowerCores ->
            "TooManyPowerCores"

        TooManyExpansionBays ->
            "TooManyExpansionBays"

        InvalidExpansionBayOnFrame ->
            "InvalidExpansionBayOnFrame"

        NotEnoughPowerForActiveSystems ->
            "NotEnoughPowerForActiveSystems"

        PowerCoreTooSmallForDriftEngines ->
            "PowerCoreTooSmallForDriftEngines"

        ShipToLargeForDriftEngine ->
            "ShipToLargeForDriftEngine"

        SpeedTooFastForSizeOrLessThan1 ->
            "SpeedTooFastForSizeOrLessThan1"


isTrue : (a -> Bool) -> b -> ( List b, a ) -> ( List b, a )
isTrue fn err ( errs, value ) =
    if fn value then
        ( errs, value )

    else
        ( err :: errs, value )


validateStarship : Starship -> List BuildError
validateStarship =
    (\b -> ( [], b ))
        >> isTrue areArcMountPointsValid TooManyWeaponMountsOnArc
        >> isTrue areTurretMountPointsValid TooManyWeaponMountsOnTurret
        >> isTrue areWeaponClassesValidForFrame InvalidWeaponClassOnFrame
        >> isTrue areTurretWeaponClassesValid IllegalCapitalWeaponOnTurret
        >> isTrue hasNoTrackingWeaponLinks IllegalLinkedTrackingWeapon
        >> isTrue hasEnoughPowerCoresForPcu PcuRequiresRequiresAdditionalPowerCore
        >> isTrue hasValidPowerCoreCount TooManyPowerCores
        >> isTrue hasValidExpansionBayCount TooManyExpansionBays
        >> isTrue isValidSizeForExpansionBays InvalidExpansionBayOnFrame
        >> isTrue hasSufficientPowerCoreUnits NotEnoughPowerForActiveSystems
        >> isTrue hasSufficientPowerCoreUnitsForDriftEngine PowerCoreTooSmallForDriftEngines
        >> isTrue isSmallEnoughForDriftEngine ShipToLargeForDriftEngine
        >> isTrue isValidSpeed SpeedTooFastForSizeOrLessThan1
        >> Tuple.first


getAcModifier : Starship -> Int
getAcModifier starship =
    let
        sizeAcBonus =
            Frame.getAcModifier starship.frame

        armorBonus =
            starship.armor
                |> Maybe.map DefenseLevel.toBonus
                |> Maybe.withDefault 0
    in
    sizeAcBonus + armorBonus


getTlModifier : Starship -> Int
getTlModifier starship =
    let
        sizeTlBonus =
            Frame.getTlModifier starship.frame

        armorTlPenalty =
            starship.armor
                |> Maybe.map DefenseLevel.toTlPenalty
                |> Maybe.withDefault 0

        countermeasuresBonus =
            starship.defensiveCountermeasures
                -- TODO: These have to be on
                |> Maybe.map (DefenseLevel.toBonus << extract)
                |> Maybe.withDefault 0
    in
    sizeTlBonus + armorTlPenalty + countermeasuresBonus


getSpeedModifier : Starship -> Int
getSpeedModifier =
    -- TODO: These have to be on
    extract << .thrusters


getDistanceBetweenTurnsModifier : Starship -> Int
getDistanceBetweenTurnsModifier starship =
    let
        frameModifier =
            Frame.getDistanceBetweenTurnsModifier starship.frame

        armorModifier =
            starship.armor
                |> Maybe.map DefenseLevel.toManeuverabilityPenalty
                |> Maybe.withDefault 0
    in
    frameModifier + armorModifier
