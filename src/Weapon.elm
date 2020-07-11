module Weapon exposing (Class(..), Irradiation(..), Range(..), Type(..), Weapon, WeaponProperty(..), getArcMountPointBuiltPoints, getTurretMountPointBuiltPoints, isTrackingWeapon, rangeToString)


type Range
    = Short
    | Medium
    | Long


rangeToString : Range -> String
rangeToString range =
    case range of
        Short ->
            "Short"

        Medium ->
            "Medium"

        Long ->
            "Long"


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
    | Burrowing
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
