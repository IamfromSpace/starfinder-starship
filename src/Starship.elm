module Starship exposing (..)

import Arc exposing (Arc)
import Weapon exposing (Weapon)
import Computer exposing (Computer)


type Size
    = Tiny
    | Small
    | Medium
    | Large
    | Huge
    | Gargantuan
    | Colossal


type Maneuverability
    = Clumsy
    | Average
    | Good
    | Perfect


type alias Frame =
    { name : String
    , size : Size
    , maneuverability : Maneuverability
    , baseHitPoints : Int
    , hitPointsIncrement : Int
    , damageThreshold : Int
    , criticalThreshold : Int
    , arcMounts : Arc (List Weapon.Class)
    , turretMounts : List Weapon.Class
    , expansionBays : Int
    , minimumCrew : Int
    , maximumCrew : Int
    , listedBuildPoints : Int
    }


getFrameBuildPoints : Frame -> Int
getFrameBuildPoints { listedBuildPoints, arcMounts, turretMounts } =
    let
        arcCosts =
            List.map Weapon.getArcMountPointBuiltPoints (Arc.concat arcMounts)

        turretCosts =
            List.map Weapon.getTurretMountPointBuiltPoints turretMounts
    in
        listedBuildPoints - List.sum (arcCosts ++ turretCosts)


getThrusterPowerDraw : Starship -> Int
getThrusterPowerDraw { frame, thrusters } =
    let
        (Togglable isOn speed) =
            thrusters
    in
        if not isOn then
            0
        else
            case frame.size of
                Tiny ->
                    round ((toFloat speed) * 2.5 + 5)

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
        (Togglable isOn speed) =
            thrusters
    in
        if not isOn then
            0
        else
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
                    round ((toFloat speed) * 0.5)


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


getArmorBuildPoints : Starship -> Int
getArmorBuildPoints { frame, armorBonus } =
    let
        costMultiplier =
            case armorBonus of
                1 ->
                    1

                2 ->
                    2

                3 ->
                    3

                4 ->
                    5

                5 ->
                    7

                6 ->
                    9

                7 ->
                    12

                8 ->
                    15

                9 ->
                    18

                10 ->
                    21

                11 ->
                    25

                12 ->
                    30

                13 ->
                    35

                14 ->
                    40

                15 ->
                    45

                _ ->
                    100000
    in
        getSizeCategory frame.size * costMultiplier


getArmorTargetLockBonus : Int -> Int
getArmorTargetLockBonus armorBonus =
    case armorBonus of
        5 ->
            -1

        6 ->
            -1

        7 ->
            -1

        8 ->
            -1

        9 ->
            -2

        10 ->
            -2

        11 ->
            -2

        12 ->
            -3

        13 ->
            -3

        14 ->
            -3

        15 ->
            -4

        _ ->
            0


type CrewQuarters
    = Common
    | GoodQuarters
    | Luxurious


getCrewQuartersBuildPoints : CrewQuarters -> Int
getCrewQuartersBuildPoints crewQuarters =
    case crewQuarters of
        Common ->
            0

        GoodQuarters ->
            2

        Luxurious ->
            5


getDefensiveCountermeasuresPowerDraw : Int -> Int
getDefensiveCountermeasuresPowerDraw =
    getDefensiveCountermeasuresBuildPoints >> (\x -> x // 2)


getDefensiveCountermeasuresBuildPoints : Int -> Int
getDefensiveCountermeasuresBuildPoints defensiveCountermeasures =
    case defensiveCountermeasures of
        0 ->
            0

        1 ->
            2

        3 ->
            4

        4 ->
            6

        5 ->
            8

        6 ->
            11

        7 ->
            14

        8 ->
            18

        9 ->
            22

        10 ->
            27

        11 ->
            33

        12 ->
            40

        13 ->
            50

        14 ->
            65

        15 ->
            90

        _ ->
            1000000


getPowerCoreUnitsBuildPoints : Int -> Int
getPowerCoreUnitsBuildPoints pcu =
    -- Totally ignoring that like 2 numbers are incorrect here, because it's stupid
    ((pcu - 1) // 10) + 1


getDriftEngineRatingBuildPoints : Starship -> Int
getDriftEngineRatingBuildPoints { frame, driftEngineRating } =
    getSizeCategory frame.size
        * case driftEngineRating of
            1 ->
                2

            2 ->
                5

            3 ->
                10

            4 ->
                15

            5 ->
                20

            _ ->
                100000


type ExpansionBay
    = ArcaneLaboratory
    | CargoHold
    | EscapePods
    | GuestQuarters
    | HangarBay
    | LifeBoats
    | MedicalBay
    | PassengerSeating
    | PowerCoreHousing
    | RecreationSuiteGym
    | RecreationSuiteTrivedDen
    | RecreationSuiteHac
    | ScienceLab
    | SealedEnvironmentChamber
    | ShuttleBay
    | SmugglerCompartment
    | SynthesisBay
    | TechWorkshop


getExpansionBayCost : ExpansionBay -> ( Int, Int )
getExpansionBayCost expansionBay =
    case expansionBay of
        ArcaneLaboratory ->
            ( 1, 1 )

        CargoHold ->
            ( 0, 0 )

        EscapePods ->
            ( 2, 1 )

        GuestQuarters ->
            ( 1, 1 )

        HangarBay ->
            ( 30, 10 )

        LifeBoats ->
            ( 5, 3 )

        MedicalBay ->
            ( 4, 8 )

        PassengerSeating ->
            ( 0, 0 )

        PowerCoreHousing ->
            ( 0, 10 )

        RecreationSuiteGym ->
            ( 0, 1 )

        RecreationSuiteTrivedDen ->
            ( 1, 1 )

        RecreationSuiteHac ->
            ( 3, 1 )

        ScienceLab ->
            ( 2, 1 )

        SealedEnvironmentChamber ->
            ( 2, 1 )

        ShuttleBay ->
            ( 10, 4 )

        SmugglerCompartment ->
            ( 4, 2 )

        SynthesisBay ->
            ( 2, 1 )

        TechWorkshop ->
            ( 3, 1 )


getExpansionBayPowerDraw : ExpansionBay -> Int
getExpansionBayPowerDraw =
    getExpansionBayCost >> Tuple.first


getExpansionBayBuildPoints : ExpansionBay -> Int
getExpansionBayBuildPoints =
    getExpansionBayCost >> Tuple.second


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


getAllWeapons : Starship -> List (Togglable Weapon)
getAllWeapons { arcWeapons, turretWeapons } =
    Arc.concat arcWeapons ++ turretWeapons


getAllOnlineWeapons : Starship -> List Weapon
getAllOnlineWeapons =
    getAllWeapons
        >> List.filterMap
            (\(Togglable isOn weapon) ->
                if isOn then
                    Just weapon
                else
                    Nothing
            )


getWeaponsPowerDraw : Starship -> Int
getWeaponsPowerDraw =
    getAllOnlineWeapons >> List.map Weapon.getPowerDraw >> List.sum


getWeaponsBuildPoints : Starship -> Int
getWeaponsBuildPoints =
    getAllWeapons >> List.map ((\(Togglable _ x) -> x) >> Weapon.getBuildPoints) >> List.sum


getMountPointsBuiltPoints : Starship -> Int
getMountPointsBuiltPoints ship =
    let
        getClass =
            (\(Togglable _ x) -> x) >> .weaponClass

        arcCost =
            List.map
                (getClass >> Weapon.getArcMountPointBuiltPoints)
                (Arc.concat ship.arcWeapons)

        turretCost =
            List.map
                (getClass >> Weapon.getTurretMountPointBuiltPoints)
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


type Togglable a
    = Togglable Bool a



--TODO: Security Systems


type alias Starship =
    { name : String
    , frame : Frame
    , powerCoreUnits :
        -- Currently ignoring that a ship can have multiple power cores
        -- (It doesn't actually make a big difference in BP)
        -- Probably better represented with a validity check for max power
        -- a craft can generate by size
        -- Tiny = 200, Small = 300, Medium = 300/600 depending on expansions, etc
        Int
    , thrusters : Togglable Int
    , armorBonus : Int
    , computer : Togglable Computer
    , crewQuarters : CrewQuarters
    , defensiveCountermeasures : Togglable Int
    , driftEngineRating : Int
    , expansionBays : List (Togglable ExpansionBay)
    , sensors : Sensor
    , arcWeapons : Arc (List (Togglable Weapon))
    , turretWeapons : List (Togglable Weapon)
    , shields : Togglable Shields
    }


getTogglablePowerDraw : (a -> Int) -> Togglable a -> Int
getTogglablePowerDraw powerDrawFn (Togglable isOn a) =
    if isOn then
        powerDrawFn a
    else
        0


getStarshipPowerDraw : Starship -> Int
getStarshipPowerDraw ship =
    getThrusterPowerDraw ship
        + getTogglablePowerDraw Computer.getPowerDraw ship.computer
        + getTogglablePowerDraw getDefensiveCountermeasuresPowerDraw ship.defensiveCountermeasures
        + List.sum (List.map (getTogglablePowerDraw getExpansionBayPowerDraw) ship.expansionBays)
        + getWeaponsPowerDraw ship
        + getTogglablePowerDraw .powerDraw ship.shields


getStarshipBuildPoints : Starship -> Int
getStarshipBuildPoints ship =
    let
        (Togglable _ computer) =
            ship.computer

        (Togglable _ defensiveCountermeasures) =
            ship.defensiveCountermeasures

        (Togglable _ shields) =
            ship.shields
    in
        getFrameBuildPoints ship.frame
            + getPowerCoreUnitsBuildPoints ship.powerCoreUnits
            + getThrusterBuildPoints ship
            + getArmorBuildPoints ship
            + Computer.getBuildPoints computer
            + getDefensiveCountermeasuresBuildPoints defensiveCountermeasures
            + getDriftEngineRatingBuildPoints ship
            + List.sum (List.map ((\(Togglable _ x) -> x) >> getExpansionBayBuildPoints) ship.expansionBays)
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


minimumPowerCoreUnitsForDriftEngineRating : Int -> Int
minimumPowerCoreUnitsForDriftEngineRating engineRating =
    case engineRating of
        1 ->
            75

        2 ->
            100

        x ->
            x * 25 + 75


maxiumumSizeForDriftEngineRating : Int -> Size
maxiumumSizeForDriftEngineRating engineRating =
    case engineRating of
        1 ->
            Colossal

        2 ->
            Huge

        3 ->
            Large

        4 ->
            Large

        _ ->
            Medium


areArcMountPointsValid : Starship -> Bool
areArcMountPointsValid { arcWeapons, frame } =
    case List.maximum (Arc.concat (Arc.map (\x -> [ List.length x ]) arcWeapons)) of
        Just max ->
            max <= getMountPointLimit frame.size

        Nothing ->
            Debug.crash "Arcs list cannot be empty!"


areTurretMountPointsValid : Starship -> Bool
areTurretMountPointsValid { turretWeapons, frame } =
    List.length turretWeapons <= getMountPointLimit frame.size


areWeaponClassesValidForFrame : Starship -> Bool
areWeaponClassesValidForFrame { arcWeapons, turretWeapons, frame } =
    List.all
        (\(Togglable _ { weaponClass }) ->
            List.member weaponClass (getAllowedClasses frame.size)
        )
        (Arc.concat arcWeapons ++ turretWeapons)


areTurretWeaponClassesValid : Starship -> Bool
areTurretWeaponClassesValid { turretWeapons, frame } =
    List.all
        (\(Togglable _ { weaponClass }) -> weaponClass /= Weapon.Capital)
        turretWeapons


hasTurretIfHasTurretWeapons : Starship -> Bool
hasTurretIfHasTurretWeapons { turretWeapons, frame } =
    List.length turretWeapons == 0 || List.length frame.turretMounts > 0


getPowerCoreCount : Starship -> Int
getPowerCoreCount { expansionBays } =
    let
        isPowerCoreHousing =
            (\(Togglable _ bay) -> bay == PowerCoreHousing)
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
    List.length expansionBays <= frame.expansionBays


hasSufficientPowerCoreUnits : Starship -> Bool
hasSufficientPowerCoreUnits ship =
    ship.powerCoreUnits >= getStarshipPowerDraw ship


hasSufficientPowerCoreUnitsForDriftEngineRating : Starship -> Bool
hasSufficientPowerCoreUnitsForDriftEngineRating ship =
    minimumPowerCoreUnitsForDriftEngineRating ship.driftEngineRating <= ship.powerCoreUnits


isSmallEnoughForDriftEngineRating : Starship -> Bool
isSmallEnoughForDriftEngineRating { driftEngineRating, frame } =
    getSizeCategory frame.size
        <= getSizeCategory (maxiumumSizeForDriftEngineRating driftEngineRating)


type BuildError
    = TooManyWeaponMountsOnArc
    | TooManyWeaponMountsOnTurret
    | InvalidWeaponClassOnFrame
    | IllegalCapitalWeaponOnTurret
    | IllegalTurretMountsOnTurretlessFrame
    | PcuRequiresRequiresAdditionalPowerCore
    | TooManyPowerCores
    | TooManyExpansionBays
    | NotEnoughPowerForActiveSystems
    | PowerCoreTooSmallForDriftEngines
    | ShipToLargeForDriftEngineRating


isTrue : (a -> Bool) -> b -> ( List b, a ) -> ( List b, a )
isTrue fn err ( errs, value ) =
    if fn value then
        ( errs, value )
    else
        ( err :: errs, value )


validateStarship : Starship -> List BuildError
validateStarship =
    (,) []
        >> isTrue areArcMountPointsValid TooManyWeaponMountsOnArc
        >> isTrue areTurretMountPointsValid TooManyWeaponMountsOnTurret
        >> isTrue areWeaponClassesValidForFrame InvalidWeaponClassOnFrame
        >> isTrue areTurretWeaponClassesValid IllegalCapitalWeaponOnTurret
        >> isTrue hasTurretIfHasTurretWeapons IllegalTurretMountsOnTurretlessFrame
        >> isTrue hasEnoughPowerCoresForPcu PcuRequiresRequiresAdditionalPowerCore
        >> isTrue hasValidPowerCoreCount TooManyPowerCores
        >> isTrue hasValidExpansionBayCount TooManyExpansionBays
        >> isTrue hasSufficientPowerCoreUnits NotEnoughPowerForActiveSystems
        >> isTrue hasSufficientPowerCoreUnitsForDriftEngineRating PowerCoreTooSmallForDriftEngines
        >> isTrue isSmallEnoughForDriftEngineRating ShipToLargeForDriftEngineRating
        >> Tuple.first
