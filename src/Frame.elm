module Frame exposing (Frame, Maneuverability(..), getAcModifier, getBuildPoints, getDistanceBetweenTurnsModifier, getPilotBonus, getTlModifier)

import Arc exposing (Arc)
import Size exposing (Size)
import Weapon exposing (Weapon)


type Maneuverability
    = Clumsy
    | Poor
    | Average
    | Good
    | Perfect


getPilotBonus : Maneuverability -> Int
getPilotBonus maneuverability =
    case maneuverability of
        Clumsy ->
            -2

        Poor ->
            -1

        Average ->
            0

        Good ->
            1

        Perfect ->
            2


type alias Frame =
    { name : String
    , size : Size
    , maneuverability : Maneuverability
    , baseHitPoints : Int
    , hitPointsIncrement : Int
    , damageThreshold : Int
    , arcMounts : Arc (List Weapon.Class)
    , turretMounts : List Weapon.Class
    , expansionBays : Int
    , minimumCrew : Int
    , maximumCrew : Int
    , listedBuildPoints : Int
    }


getBuildPoints : Frame -> Int
getBuildPoints { listedBuildPoints, arcMounts, turretMounts } =
    let
        arcCosts =
            List.map Weapon.getArcMountPointBuiltPoints (Arc.concat arcMounts)

        turretCosts =
            List.map Weapon.getTurretMountPointBuiltPoints turretMounts
    in
    listedBuildPoints - List.sum (arcCosts ++ turretCosts)


getAcModifier : Frame -> Int
getAcModifier =
    Size.getAcModifier << .size


getTlModifier : Frame -> Int
getTlModifier =
    Size.getTlModifier << .size


getDistanceBetweenTurnsModifier : Frame -> Int
getDistanceBetweenTurnsModifier frame =
    case frame.maneuverability of
        Clumsy ->
            4

        Poor ->
            3

        Average ->
            2

        Good ->
            1

        Perfect ->
            0
