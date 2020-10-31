module Crewmate exposing (Crewmate, getAcModifier, getTlModifier)


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
