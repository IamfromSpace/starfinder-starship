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

    -- None means "infinity" or "no turns allowed"
    , maneuverabilityDelta : Maybe Int
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


getDistanceBetweenTurnsModifier : PilotResult -> Maybe Int
getDistanceBetweenTurnsModifier =
    .maneuverabilityDelta


noPilotResult : PilotResult
noPilotResult =
    { acAndTlBonus = 0
    , speedDelta = 0
    , maneuverabilityDelta = Just 0
    , special = Nothing
    }



-- TODO: Nothing here really needs a Starship, these need the ship's speed


getDeltaToHalfSpeed : Starship -> Int
getDeltaToHalfSpeed starship =
    let
        baseSpeed =
            -- TODO: this ignores if thrusters are off
            extract starship.thrusters

        finalSpeed =
            baseSpeed // 2
    in
    finalSpeed - baseSpeed


maneuver : PilotResult
maneuver =
    { noPilotResult | maneuverabilityDelta = Just -1 }


backOff : Starship -> PilotResult
backOff starship =
    { noPilotResult
        | maneuverabilityDelta = Nothing
        , speedDelta = getDeltaToHalfSpeed starship
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
    { noPilotResult
        | speedDelta = getDeltaToHalfSpeed starship
        , special = Just SwapPortAndStarboard
    }


barrelRollFail : Starship -> PilotResult
barrelRollFail starship =
    { noPilotResult
        | speedDelta = getDeltaToHalfSpeed starship
    }


barrelRollFailBy5OrMore : Starship -> PilotResult
barrelRollFailBy5OrMore starship =
    { noPilotResult
        | acAndTlBonus = -4
        , speedDelta = getDeltaToHalfSpeed starship
    }


evade : PilotResult
evade =
    { noPilotResult | acAndTlBonus = 2 }


evadeFailBy5OrMore : PilotResult
evadeFailBy5OrMore =
    { noPilotResult | acAndTlBonus = -2 }


flipAndBurn : Starship -> PilotResult
flipAndBurn starship =
    { noPilotResult
        | maneuverabilityDelta = Nothing
        , speedDelta = getDeltaToHalfSpeed starship
        , special = Just Turn180AtTheEnd
    }


flipAndBurnFail : Starship -> PilotResult
flipAndBurnFail starship =
    { noPilotResult
        | maneuverabilityDelta = Nothing
        , speedDelta = getDeltaToHalfSpeed starship
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
    { noPilotResult
        | maneuverabilityDelta = Nothing
        , speedDelta = getDeltaToHalfSpeed starship
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
        , special = Just TurnInPlace
    }


fullPower : Starship -> PilotResult
fullPower starship =
    -- TODO: this ignores if thrusters are off
    { noPilotResult
        | maneuverabilityDelta = Just 2
        , speedDelta = extract starship.thrusters // 2
    }


audaciousGambit : PilotResult
audaciousGambit =
    { noPilotResult
        | maneuverabilityDelta = Just -2
        , special = Just NoFreeAttackForAnyEnemyAndFreeFinalRotation
    }


audaciousGambitFail : PilotResult
audaciousGambitFail =
    -- TODO: Also costs a Resolve Point
    noPilotResult
