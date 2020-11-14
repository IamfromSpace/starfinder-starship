module CrewmateStatus exposing (CrewmateStatus, demandSource, demandTarget, encourageSource, encourageTarget, getBluffSkillModifier, getComputersSkillModifier, getDiplomacySkillModifier, getEngineeringSkillModifier, getGunningModifier, getIntimidateSkillModifier, getPilotingSkillModifier, movingSpeechSource, movingSpeechTarget, ordersSource, ordersTarget, tauntSource)

import Arc exposing (AnArc)
import Crewmate exposing (Crewmate)
import PatchableSystems exposing (PatchableSystem(..))
import Set exposing (Set)
import Starship exposing (Starship)


type alias CrewmateStatus =
    { resolvePoints : Int
    , crewDemanded : Set String
    , demanded : Bool -- TODO: Lasts one round
    , encouraged : Bool -- TODO: Lasts one round
    , ordered : Bool -- TODO: Lasts one round
    , moved : Bool -- TODO: Lasts only one phase
    , staminaPoints : Int
    , hitPoints : Int
    , actions : Int -- TODO: Counter for one round
    }


baseBonusModifier : CrewmateStatus -> Int
baseBonusModifier { demanded, encouraged } =
    let
        d =
            if demanded then
                4

            else
                0

        e =
            if encouraged then
                2

            else
                0
    in
    d + e


getGunningModifier : CrewmateStatus -> Int
getGunningModifier =
    baseBonusModifier


getPilotingSkillModifier : CrewmateStatus -> Int
getPilotingSkillModifier =
    baseBonusModifier


getEngineeringSkillModifier : CrewmateStatus -> Int
getEngineeringSkillModifier =
    baseBonusModifier


getComputersSkillModifier : CrewmateStatus -> Int
getComputersSkillModifier =
    baseBonusModifier


getDiplomacySkillModifier : CrewmateStatus -> Int
getDiplomacySkillModifier =
    baseBonusModifier


getIntimidateSkillModifier : CrewmateStatus -> Int
getIntimidateSkillModifier =
    baseBonusModifier


getBluffSkillModifier : CrewmateStatus -> Int
getBluffSkillModifier =
    baseBonusModifier


canAct : CrewmateStatus -> Bool
canAct { actions, ordered } =
    actions == 0 || ordered && actions == 1


demandSource : CrewmateStatus -> { a | target : String } -> Maybe CrewmateStatus
demandSource cms { target } =
    if Set.member target cms.crewDemanded && canAct cms then
        Nothing

    else
        Just
            { cms
                | crewDemanded = Set.insert target cms.crewDemanded
                , actions = cms.actions + 1
            }


demandTarget : CrewmateStatus -> CrewmateStatus
demandTarget cms =
    { cms | demanded = True }


encourageSource : CrewmateStatus -> Maybe CrewmateStatus
encourageSource cms =
    if canAct cms then
        Just { cms | actions = cms.actions + 1 }

    else
        Nothing


encourageTarget : CrewmateStatus -> CrewmateStatus
encourageTarget cms =
    { cms | encouraged = True }


tauntSource : CrewmateStatus -> Maybe CrewmateStatus
tauntSource cms =
    if canAct cms then
        Just { cms | actions = cms.actions + 1 }

    else
        Nothing


ordersSource : CrewmateStatus -> Maybe CrewmateStatus
ordersSource cms =
    if cms.resolvePoints > 0 && canAct cms then
        Just
            { cms
                | resolvePoints = cms.resolvePoints - 1
                , actions = cms.actions + 1
            }

    else
        Nothing


ordersTarget : CrewmateStatus -> CrewmateStatus
ordersTarget cms =
    { cms | ordered = True }


movingSpeechSource : CrewmateStatus -> Maybe CrewmateStatus
movingSpeechSource cms =
    if cms.resolvePoints > 0 && canAct cms then
        Just
            { cms
                | resolvePoints = cms.resolvePoints - 1
                , actions = cms.actions + 1
            }

    else
        Nothing



-- TODO: Since we affect _all_ crew aboard, it may more be better to put this
-- on the Status (though it could be denormalized in our HTTP response).


movingSpeechTarget : CrewmateStatus -> CrewmateStatus
movingSpeechTarget cms =
    { cms | moved = True }
