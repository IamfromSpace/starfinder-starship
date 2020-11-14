module Crewmate exposing (Crewmate, getAcModifier, getBluffSkillModifier, getComputersSkillModifier, getDiplomacySkillModifier, getEngineeringSkillModifier, getGunningModifier, getIntimidateSkillModifier, getPilotingSkillModifier, getTlModifier, movingSpeechSource, ordersSource)


type alias Crewmate =
    { level : Int
    , baseAttackBonus : Int
    , dexterityBonus : Int
    , reflexSave : Int
    , pilotingRanks : Int
    , pilotingSkillBonus : Int
    , engineeringRanks : Int
    , engineeringSkillBonus : Int
    , computersRanks : Int
    , computersSkillBonus : Int
    , diplomacySkillBonus : Int
    , intimidateSkillBonus : Int
    , bluffSkillBonus : Int
    , initialStaminaPoints : Int
    , initialHitPoints : Int
    }


getAcModifier : Crewmate -> Int
getAcModifier =
    .pilotingRanks


getTlModifier : Crewmate -> Int
getTlModifier =
    .pilotingRanks


getGunningModifier : Crewmate -> Int
getGunningModifier cm =
    max cm.baseAttackBonus cm.pilotingSkillBonus


getPilotingSkillModifier : Crewmate -> Int
getPilotingSkillModifier =
    .pilotingSkillBonus


getEngineeringSkillModifier : Crewmate -> Int
getEngineeringSkillModifier =
    .engineeringSkillBonus


getComputersSkillModifier : Crewmate -> Int
getComputersSkillModifier =
    .computersSkillBonus


getDiplomacySkillModifier : Crewmate -> Int
getDiplomacySkillModifier =
    .diplomacySkillBonus


getIntimidateSkillModifier : Crewmate -> Int
getIntimidateSkillModifier =
    .intimidateSkillBonus


getBluffSkillModifier : Crewmate -> Int
getBluffSkillModifier =
    .bluffSkillBonus


ordersSource : Crewmate -> Maybe Crewmate
ordersSource ({ level } as cm) =
    if level >= 6 then
        Just cm

    else
        Nothing


movingSpeechSource : Crewmate -> Maybe Crewmate
movingSpeechSource ({ level } as cm) =
    if level >= 12 then
        Just cm

    else
        Nothing
