module Weapon exposing (..)


type Range
    = Short
    | Medium
    | Long


type Class
    = Light
    | Heavy
    | Capital


type Type
    = DirectFire
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


isTrackingWeapon : Weapon -> Bool
isTrackingWeapon weapon =
    case weapon.weaponType of
        Tracking _ ->
            True

        _ ->
            False
