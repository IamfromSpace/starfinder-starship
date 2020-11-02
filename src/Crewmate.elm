module Crewmate exposing (Crewmate, getAcModifier, getBluffSkillModifier, getComputersSkillModifier, getDiplomacySkillModifier, getEngineeringSkillModifier, getGunningModifier, getIntimidateSkillModifier, getPilotingSkillModifier, getTlModifier)


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
