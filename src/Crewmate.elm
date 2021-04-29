module Crewmate exposing (Crewmate, backOff, barrelRoll, evade, flipAndBurn, flyby, fullPower, getAcModifier, getBluffSkillModifier, getComputersSkillModifier, getEngineeringSkillModifier, getGunningModifier, getIntimidateSkillModifier, getPilotingSkillModifier, getTlModifier, init, maneuver, movingSpeechSource, ordersSource, slide, turnInPlace)


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


init : Crewmate
init =
    { level = 1
    , baseAttackBonus = 0
    , dexterityBonus = 0
    , reflexSave = 0
    , pilotingRanks = 0
    , pilotingSkillBonus = 0
    , engineeringRanks = 0
    , engineeringSkillBonus = 0
    , computersRanks = 0
    , computersSkillBonus = 0
    , diplomacySkillBonus = 0
    , intimidateSkillBonus = 0
    , bluffSkillBonus = 0
    , initialStaminaPoints = 0
    , initialHitPoints = 0
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


maneuver : Crewmate -> Int
maneuver =
    getPilotingSkillModifier


backOff : Crewmate -> Int
backOff =
    getPilotingSkillModifier


barrelRoll : Crewmate -> Int
barrelRoll =
    getPilotingSkillModifier


evade : Crewmate -> Int
evade =
    getPilotingSkillModifier


flipAndBurn : Crewmate -> Int
flipAndBurn =
    getPilotingSkillModifier


flyby : Crewmate -> Int
flyby =
    getPilotingSkillModifier


slide : Crewmate -> Int
slide =
    getPilotingSkillModifier


turnInPlace : Crewmate -> Int
turnInPlace =
    getPilotingSkillModifier


fullPower : Crewmate -> Maybe Int
fullPower ({ pilotingRanks } as cm) =
    if pilotingRanks >= 6 then
        Just <| getPilotingSkillModifier cm

    else
        Nothing


ordersSource : Crewmate -> Maybe Crewmate
ordersSource ({ level } as cm) =
    if level >= 6 then
        Just cm

    else
        Nothing


movingSpeechSource : Crewmate -> Maybe Int
movingSpeechSource ({ level } as cm) =
    if level >= 12 then
        Just (getDiplomacySkillModifier cm)

    else
        Nothing
