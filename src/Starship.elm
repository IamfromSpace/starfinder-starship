module Main exposing (..)


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


type alias Arc a =
    { forward : a
    , aft : a
    , portSide : a
    , starboard : a
    }


mapArc : (a -> b) -> Arc a -> Arc b
mapArc fn arcs =
    { forward = fn arcs.forward
    , aft = fn arcs.aft
    , portSide = fn arcs.portSide
    , starboard = fn arcs.starboard
    }


appendArcs : Arc appendable -> appendable
appendArcs arcs =
    arcs.forward
        ++ arcs.aft
        ++ arcs.portSide
        ++ arcs.starboard


type alias Frame =
    { name : String
    , size : Size
    , maneuverability : Maneuverability
    , maxHitPoints : Int
    , damageThreshold : Int
    , criticalThreshold : Int
    , arcMounts : Arc (List WeaponClass)
    , turretMounts : List WeaponClass
    , expansionBays : Int
    , minimumCrew : Int
    , maximumCrew : Int
    , listedBuildPoints : Int
    }


getFrameBuildPoints : Frame -> Int
getFrameBuildPoints { listedBuildPoints, arcMounts, turretMounts } =
    listedBuildPoints
        - List.foldr (getMountPointBuiltPoints >> (+)) 0 (appendArcs arcMounts ++ turretMounts)
        - (2 * (List.length turretMounts))


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


getComputerPowerDraw : Computer -> Int
getComputerPowerDraw { bonus } =
    bonus * 5 + 5


getComputerBuildPoints : Computer -> Int
getComputerBuildPoints { bonus, nodes } =
    bonus * bonus * nodes


type alias Computer =
    { bonus : Int
    , nodes : Int
    }


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
getPowerCoreUnitsBuildPoints =
    -- Totally ignoring that like 2 numbers are incorrect here, because it's stupid
    flip (//) 10


getDriftEngineBuildPoints : Starship -> Int
getDriftEngineBuildPoints { frame, driftEngine } =
    getSizeCategory frame.size
        * case driftEngine of
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


type Range
    = Short
    | MediumRange
    | Long


type alias Sensor =
    { range : Range
    , bonus : Int
    }


getSensorBuildPoints : Sensor -> Int
getSensorBuildPoints { range, bonus } =
    case range of
        Short ->
            bonus // 2 + 2

        MediumRange ->
            bonus + 3

        Long ->
            bonus * 2 + 6


type WeaponClass
    = Light
    | Heavy
    | Capital


type
    WeaponType
    -- Direct Fire weapon can be linked, for double the effects
    -- costing 2 mounts and 2.5x the build points
    = DirectFire Bool
      -- Tracking weapons have a speed of the tracking projectile
    | Tracking Int


type Irradiation
    = High
    | MediumIrradiation
    | Low


type WeaponProperty
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


type alias Weapon =
    { name : String
    , range : Range
    , weaponClass : WeaponClass
    , weaponType : WeaponType
    , damage : Maybe ( Int, Int )
    , powerDraw : Int
    , buildPoints : Int
    , specialProperties : List WeaponProperty
    }


getWeaponPowerDraw : Weapon -> Int
getWeaponPowerDraw weapon =
    (*) weapon.powerDraw <|
        case weapon.weaponType of
            DirectFire True ->
                2

            _ ->
                1


getWeaponBuildPoints : Weapon -> Int
getWeaponBuildPoints weapon =
    weapon.buildPoints
        |> toFloat
        |> (*)
            (case weapon.weaponType of
                DirectFire True ->
                    2.5

                _ ->
                    1
            )
        |> round


getAllWeapons : Starship -> List (Togglable Weapon)
getAllWeapons { arcWeapons, turretWeapons } =
    appendArcs arcWeapons ++ turretWeapons


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
    getAllOnlineWeapons >> List.foldr (getWeaponPowerDraw >> (+)) 0


getWeaponsBuildPoints : Starship -> Int
getWeaponsBuildPoints =
    getAllWeapons >> List.foldr ((\(Togglable _ x) -> x) >> getWeaponBuildPoints >> (+)) 0


getMountPointBuiltPoints : WeaponClass -> Int
getMountPointBuiltPoints weaponClass =
    case weaponClass of
        Light ->
            3

        Heavy ->
            4

        Capital ->
            5


getMountPointsBuiltPoints : Starship -> Int
getMountPointsBuiltPoints ship =
    List.foldr ((\(Togglable _ x) -> x) >> .weaponClass >> getMountPointBuiltPoints >> (+)) 0 (getAllWeapons ship)
        + (2 * List.length ship.turretWeapons)


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
    , driftEngine : Int
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
        + getTogglablePowerDraw getComputerPowerDraw ship.computer
        + getTogglablePowerDraw getDefensiveCountermeasuresPowerDraw ship.defensiveCountermeasures
        + List.foldr ((getTogglablePowerDraw getExpansionBayPowerDraw) >> (+)) 0 ship.expansionBays
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
            + getComputerBuildPoints computer
            + getDefensiveCountermeasuresBuildPoints defensiveCountermeasures
            + getDriftEngineBuildPoints ship
            + List.foldr ((\(Togglable _ x) -> x) >> getExpansionBayBuildPoints >> (+)) 0 ship.expansionBays
            + getSensorBuildPoints ship.sensors
            + getWeaponsBuildPoints ship
            + getMountPointsBuiltPoints ship
            + shields.buildPoints



-- Validate Arc Mounted/Turret Mounted Weapon Count
-- Validate Turret Mounted Class (no Capital weapons)
-- Validate Power Core Count agains max per size + allowed expansions
-- Validate ExpansionBay Count
-- Validate PCU Total against max per size + expansions
-- Validate total power draw
-- Validate Drift Engines (against miminum PCU)
