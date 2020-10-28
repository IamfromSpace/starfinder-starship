module CrewmateStatus exposing (CrewmateStatus)

import Arc exposing (AnArc)
import Crewmate exposing (Crewmate)
import PatchableSystems exposing (PatchableSystem(..))
import Starship exposing (Starship)
import Status exposing (Status)


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



-- TODO: It may make sense to calculate _both_ the bonus and DC at the same
-- time.  The benefit is that it's easier to associate the two, and some of
-- these are directly tied together.
--
-- TODO: Computer nodes can be consumed to gain additional bonuses
--
-- TODO: Bonuses return as Nothing if the action cannot be performed.  If the
-- action has already been performed this round (for actions that can only be
-- taken once), should it also return Nothing? Or what if the crewmate has
-- already taken an action?


standardBonus : PatchableSystem -> (Crewmate -> Int) -> (Crewmate -> Bool) -> Bool -> Bool -> Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
standardBonus involvedSystem getApplicableSkillBonus checkQualifications isPush requiresRP crewmate crewmateStatus currentRound status =
    if checkQualifications crewmate && (crewmateStatus.resolvePoints > 0 || not requiresRP) then
        Status.getEffectiveBonus currentRound involvedSystem isPush status
            |> Maybe.map ((+) (baseBonusModifier crewmateStatus + getApplicableSkillBonus crewmate))

    else
        Nothing


standardCaptainBonus : (Int -> Bool) -> Bool -> Bool -> Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
standardCaptainBonus checkQualification =
    standardBonus LifeSupport .diplomacySkillBonus (.level >> checkQualification)


demandBonus : Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
demandBonus =
    standardCaptainBonus (always True) False False



-- TODO: Encourage is interesting, because you can essentially get a +5 (DC
-- reduced by 5) for using the associated skill instead of diplomacy


tauntBonus : Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
tauntBonus =
    standardCaptainBonus (always True) True False



-- TODO: Orders is interesting, because you need to use the skill that target
-- character will use


movingSpeechBonus : Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
movingSpeechBonus =
    standardCaptainBonus (\x -> x >= 12) True True


standardEngineerBonus : (Int -> Bool) -> Bool -> Bool -> Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
standardEngineerBonus checkQualification =
    standardBonus PowerCore .engineeringSkillBonus (.engineeringRanks >> checkQualification)


divertBonus : Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
divertBonus =
    standardEngineerBonus (always True) False False


holdItTogetherBonus : Crewmate -> CrewmateStatus -> Int
holdItTogetherBonus crewmate crewmateStatus =
    baseBonusModifier crewmateStatus + crewmate.engineeringSkillBonus


patchBonus : Crewmate -> CrewmateStatus -> Int
patchBonus crewmate crewmateStatus =
    -- TODO: But there are multiple ways to patch with different DCs
    baseBonusModifier crewmateStatus + crewmate.engineeringSkillBonus


overpowerBonus : Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
overpowerBonus =
    standardEngineerBonus (\x -> x >= 6) True True


quickFixBonus : Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
quickFixBonus =
    standardEngineerBonus (\x -> x >= 12) True True



-- TODO: The science officer's "lock-on" grants a +2 bonus to the enemy target
-- TODO: Range penalties are unique by both weapon and distance


standardGunnerBonus : (Int -> Bool) -> Bool -> Bool -> Int -> AnArc -> Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
standardGunnerBonus checkQualification isPush requiresRP otherBonus arc crewmate crewmateStatus currentRound status =
    let
        associatedSkill cm =
            cm.dexterityBonus
                + max cm.baseAttackBonus cm.pilotingSkillBonus

        beforeOtherBonus =
            standardBonus (WeaponsArray arc) associatedSkill (.level >> checkQualification) isPush requiresRP crewmate crewmateStatus currentRound status
    in
    Maybe.map ((+) otherBonus) beforeOtherBonus


fireAtWillBonus : AnArc -> Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
fireAtWillBonus =
    standardGunnerBonus (always True) True False -4


shootBonus : AnArc -> Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
shootBonus =
    standardGunnerBonus (always True) False False 0


broadsideBonus : AnArc -> Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
broadsideBonus =
    standardGunnerBonus (\x -> x >= 6) True True -2


preciseTargetingBonus : AnArc -> Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
preciseTargetingBonus =
    standardGunnerBonus (\x -> x >= 12) False True 0


standardPilotBonus : (Int -> Bool) -> Bool -> Bool -> Starship -> Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
standardPilotBonus checkQualification isPush requiresRP starship crewmate crewmateStatus rountNumber status =
    let
        starshipBonus =
            Starship.getPilotBonus starship

        standardBonus_ =
            standardBonus Engines .pilotingSkillBonus (.pilotingRanks >> checkQualification) isPush requiresRP crewmate crewmateStatus rountNumber status
    in
    Maybe.map ((+) starshipBonus) standardBonus_


maneuverBonus : Starship -> Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
maneuverBonus =
    standardPilotBonus (always True) False False


stuntBonus : Starship -> Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
stuntBonus =
    standardPilotBonus (always True) True False


fullPowerBonus : Starship -> Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
fullPowerBonus =
    standardPilotBonus (\x -> x <= 6) True True


audaciousGambitBonus : Starship -> Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
audaciousGambitBonus =
    standardPilotBonus (\x -> x <= 12) True True


standardScienceOfficerBonus : (Int -> Bool) -> Bool -> Bool -> Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
standardScienceOfficerBonus checkQualification isPush requiresRP crewmate crewmateStatus currentRound status =
    let
        withoutDivertBonus =
            -- TODO: Third time that we're almost entirely bypassing our
            -- `standardBonus` abstraction
            standardBonus Sensors .computersSkillBonus (.computersRanks >> checkQualification) isPush requiresRP crewmate crewmateStatus currentRound status

        bonusFromDivert =
            if Status.hasExtraPower Status.ScienceEquipment currentRound status then
                2

            else
                0
    in
    Maybe.map ((+) bonusFromDivert) withoutDivertBonus


balanceBonus : Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
balanceBonus =
    standardScienceOfficerBonus (always True) False False


scanBonus : Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
scanBonus =
    -- TODO: But you learn more depending on result above DC
    standardScienceOfficerBonus (always True) False False


targetSystemBonus : Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
targetSystemBonus =
    standardScienceOfficerBonus (always True) True False


lockOnBonus : Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
lockOnBonus =
    standardScienceOfficerBonus (\x -> x >= 6) True True


improveCountermeasuresBonus : Crewmate -> CrewmateStatus -> Int -> Status -> Maybe Int
improveCountermeasuresBonus =
    standardScienceOfficerBonus (\x -> x >= 12) True True
