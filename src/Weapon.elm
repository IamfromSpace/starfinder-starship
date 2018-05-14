module Weapon exposing (..)


type Range
    = Short
    | Medium
    | Long


type Class
    = Light
    | Heavy
    | Capital


type
    Type
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
    , weaponClass : Class
    , weaponType : Type
    , damage : Maybe ( Int, Int )
    , powerDraw : Int
    , buildPoints : Int
    , specialProperties : List WeaponProperty
    }


getPowerDraw : Weapon -> Int
getPowerDraw weapon =
    (*) weapon.powerDraw <|
        case weapon.weaponType of
            DirectFire True ->
                2

            _ ->
                1


getBuildPoints : Weapon -> Int
getBuildPoints weapon =
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


getArcMountPointBuiltPoints : Class -> Int
getArcMountPointBuiltPoints weaponClass =
    case weaponClass of
        Light ->
            3

        Heavy ->
            7

        Capital ->
            12


getTurretMountPointBuiltPoints : Class -> Int
getTurretMountPointBuiltPoints weaponClass =
    case weaponClass of
        Light ->
            5

        Heavy ->
            11

        Capital ->
            -- TODO: These scenarios should probably be Maybes
            100000
