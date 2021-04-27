module PilotResult exposing (PilotResult, SpecialPilotResult(..), audaciousGambit, audaciousGambitFail, backOff, backOffFail, backOffFailBy5OrMore, barrelRoll, barrelRollFail, barrelRollFailBy5OrMore, evade, evadeFailBy5OrMore, flipAndBurn, flipAndBurnFail, flyby, flybyFail, fullPower, getAcModifier, getDistanceBetweenTurnsModifier, getSpeedModifier, getTlModifier, maneuver, noPilotResult, slide, slideFail, turnInPlace)

import Frame exposing (Maneuverability(..))
import Starship exposing (Starship)
import Togglable exposing (extract)


type SpecialPilotResult
    = MoveForwardFullSpeed
    | Turn180AtTheEnd
    | MoveBackwardExactlyOneHex
    | MoveBackward
    | SwapPortAndStarboard
    | NoFreeAttackForSingleEnemy
    | NoFreeAttackForAnyEnemyAndFreeFinalRotation
    | Slide
    | TurnInPlace


type alias PilotResult =
    { acAndTlBonus : Int
    , speedDelta : Int
    , maneuverabilityDelta : Int
    , special : Maybe SpecialPilotResult
    }


getAcModifier : PilotResult -> Int
getAcModifier =
    .acAndTlBonus


getTlModifier : PilotResult -> Int
getTlModifier =
    .acAndTlBonus


getSpeedModifier : PilotResult -> Int
getSpeedModifier =
    .speedDelta


getDistanceBetweenTurnsModifier : PilotResult -> Int
getDistanceBetweenTurnsModifier =
    .maneuverabilityDelta


noPilotResult : PilotResult
noPilotResult =
    { acAndTlBonus = 0
    , speedDelta = 0
    , maneuverabilityDelta = 0
    , special = Nothing
    }



-- TODO: Nothing here really needs a Starship, these need the ship's speed


reductionToHalfSpeedAndNoTurns : Starship -> ( Int, Int )
reductionToHalfSpeedAndNoTurns starship =
    let
        baseSpeed =
            -- TODO: this ignores if thrusters are off
            extract starship.thrusters

        finalSpeed =
            baseSpeed // 2
    in
    -- TODO: This also considers if power has been diverted to the engines (by
    -- adding 3 instead of 1) but ultimately this should just be represented as
    -- a Maybe or something.
    ( finalSpeed - baseSpeed, finalSpeed + 3 )


maneuver : PilotResult
maneuver =
    { noPilotResult | maneuverabilityDelta = -1 }


backOff : Starship -> PilotResult
backOff starship =
    let
        ( speedDelta, maneuverabilityDelta ) =
            reductionToHalfSpeedAndNoTurns starship
    in
    { noPilotResult
        | maneuverabilityDelta = maneuverabilityDelta
        , speedDelta = speedDelta
        , special = Just MoveBackward
    }


backOffFail : PilotResult
backOffFail =
    { noPilotResult | special = Just MoveBackwardExactlyOneHex }


backOffFailBy5OrMore : Starship -> PilotResult
backOffFailBy5OrMore starship =
    { noPilotResult
        | acAndTlBonus = -4

        -- TODO: this ignores if thrusters are off
        , speedDelta = -(extract starship.thrusters)
    }


barrelRoll : Starship -> PilotResult
barrelRoll starship =
    -- TODO: Only valid for ships Large or smaller
    { noPilotResult
        | speedDelta =
            Tuple.first <|
                reductionToHalfSpeedAndNoTurns starship
        , special = Just SwapPortAndStarboard
    }


barrelRollFail : Starship -> PilotResult
barrelRollFail starship =
    -- TODO: Only valid for ships Large or smaller
    { noPilotResult
        | speedDelta =
            Tuple.first <|
                reductionToHalfSpeedAndNoTurns starship
    }


barrelRollFailBy5OrMore : Starship -> PilotResult
barrelRollFailBy5OrMore starship =
    -- TODO: Only valid for ships Large or smaller
    { noPilotResult
        | acAndTlBonus = -4
        , speedDelta =
            Tuple.first <|
                reductionToHalfSpeedAndNoTurns starship
    }


evade : PilotResult
evade =
    { noPilotResult | acAndTlBonus = 2 }


evadeFailBy5OrMore : PilotResult
evadeFailBy5OrMore =
    { noPilotResult | acAndTlBonus = -2 }


flipAndBurn : Starship -> PilotResult
flipAndBurn starship =
    let
        ( speedDelta, maneuverabilityDelta ) =
            reductionToHalfSpeedAndNoTurns starship
    in
    { noPilotResult
        | maneuverabilityDelta = maneuverabilityDelta
        , speedDelta = speedDelta
        , special = Just Turn180AtTheEnd
    }


flipAndBurnFail : Starship -> PilotResult
flipAndBurnFail starship =
    let
        ( speedDelta, maneuverabilityDelta ) =
            reductionToHalfSpeedAndNoTurns starship
    in
    { noPilotResult
        | maneuverabilityDelta = maneuverabilityDelta
        , speedDelta = speedDelta
        , special = Just MoveForwardFullSpeed
    }


flyby : PilotResult
flyby =
    { noPilotResult | special = Just NoFreeAttackForSingleEnemy }


flybyFail : PilotResult
flybyFail =
    noPilotResult


slide : PilotResult
slide =
    { noPilotResult | special = Just Slide }


slideFail : Starship -> PilotResult
slideFail starship =
    let
        ( speedDelta, maneuverabilityDelta ) =
            reductionToHalfSpeedAndNoTurns starship
    in
    { noPilotResult
        | maneuverabilityDelta = maneuverabilityDelta
        , speedDelta = speedDelta
    }


turnInPlace : Starship -> PilotResult
turnInPlace starship =
    { noPilotResult
        | acAndTlBonus =
            case starship.frame.maneuverability of
                Clumsy ->
                    -4

                Poor ->
                    -2

                _ ->
                    0

        -- TODO: this ignores if thrusters are off
        , speedDelta = -(extract starship.thrusters)
    }


fullPower : Starship -> PilotResult
fullPower starship =
    -- TODO: Also costs a Resolve Point
    -- TODO: this ignores if thrusters are off
    { noPilotResult
        | maneuverabilityDelta = 2
        , speedDelta = extract starship.thrusters // 3
    }


audaciousGambit : PilotResult
audaciousGambit =
    -- TODO: Also costs a Resolve Point
    { noPilotResult
        | maneuverabilityDelta = -2
        , special = Just NoFreeAttackForAnyEnemyAndFreeFinalRotation
    }


audaciousGambitFail : PilotResult
audaciousGambitFail =
    -- TODO: Also costs a Resolve Point
    noPilotResult
