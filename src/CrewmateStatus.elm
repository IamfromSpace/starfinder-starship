module CrewmateStatus exposing (CrewmateStatus, getBluffSkillModifier, getComputersSkillModifier, getDiplomacySkillModifier, getEngineeringSkillModifier, getGunningModifier, getIntimidateSkillModifier, getPilotingSkillModifier)

import Arc exposing (AnArc)
import Crewmate exposing (Crewmate)
import PatchableSystems exposing (PatchableSystem(..))
import Starship exposing (Starship)


type alias CrewmateStatus =
    { resolvePoints : Int
    , demanded : Bool -- TODO: Can only be demanded by one PC one time
    , encouraged : Bool
    , ordered : Bool
    , moved : Bool
    , staminaPoints : Int
    , hitPoints : Int
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
