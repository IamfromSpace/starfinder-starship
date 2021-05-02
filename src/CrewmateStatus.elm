module CrewmateStatus exposing (CrewmateStatus, audaciousGambit, backOff, barrelRoll, demandSource, demandTarget, encourageSource, encourageTarget, evade, flipAndBurn, flyby, fullPower, getBluffSkillModifier, getComputersSkillModifier, getEngineeringSkillModifier, getGunningModifier, getIntimidateSkillModifier, getPilotingSkillModifier, init, maneuver, movingSpeechSource, movingSpeechTarget, ordersSource, ordersTarget, slide, tauntSource, turnInPlace)

import Arc exposing (AnArc)
import CombatPhase exposing (CombatPhase(..))
import Crewmate exposing (Crewmate)
import PatchableSystems exposing (PatchableSystem(..))
import Set exposing (Set)
import Starship exposing (Starship)



-- TODO: This is actually a ship wide effect.  Sigh.


type alias PhaseStatus =
    { moved : Bool
    }


type alias RoundStatus =
    { demanded : Bool
    , encouraged : Bool
    , ordered : Bool
    , actions : Int
    , phaseStatus : ( CombatPhase, PhaseStatus )
    }


type alias CrewmateStatus =
    { resolvePoints : Int
    , crewDemanded : Set String
    , staminaPoints : Int
    , hitPoints : Int
    , roundStatus : ( Int, RoundStatus )
    }


initPS : PhaseStatus
initPS =
    { moved = False
    }


initRS : RoundStatus
initRS =
    { demanded = False
    , encouraged = False
    , ordered = False
    , actions = 0
    , phaseStatus =
        ( Engineering
        , initPS
        )
    }


init : CrewmateStatus
init =
    -- TODO: This isn't ideal, only some of these fields really have reasonable
    -- defaults
    { resolvePoints = 10000000
    , crewDemanded = Set.empty
    , staminaPoints = 10000000
    , hitPoints = 10000000
    , roundStatus = ( -1, initRS )
    }


baseBonusModifier : CrewmateStatus -> { a | currentRound : Int } -> Int
baseBonusModifier cs { currentRound } =
    let
        ( roundApplied, { demanded, encouraged } ) =
            cs.roundStatus

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
    if roundApplied == currentRound then
        d + e

    else
        0


getGunningModifier : CrewmateStatus -> { a | currentRound : Int } -> Int
getGunningModifier =
    baseBonusModifier


getPilotingSkillModifier : CrewmateStatus -> { a | currentRound : Int } -> Int
getPilotingSkillModifier =
    baseBonusModifier


getEngineeringSkillModifier : CrewmateStatus -> { a | currentRound : Int } -> Int
getEngineeringSkillModifier =
    baseBonusModifier


getComputersSkillModifier : CrewmateStatus -> { a | currentRound : Int } -> Int
getComputersSkillModifier =
    baseBonusModifier


getDiplomacySkillModifier : CrewmateStatus -> { a | currentRound : Int } -> Int
getDiplomacySkillModifier =
    baseBonusModifier


getIntimidateSkillModifier : CrewmateStatus -> { a | currentRound : Int } -> Int
getIntimidateSkillModifier =
    baseBonusModifier


getBluffSkillModifier : CrewmateStatus -> { a | currentRound : Int } -> Int
getBluffSkillModifier =
    baseBonusModifier


getCurrentRoundStatus : CrewmateStatus -> { a | currentRound : Int } -> RoundStatus
getCurrentRoundStatus cms { currentRound } =
    let
        ( roundAffected, lastRoundStatus ) =
            cms.roundStatus
    in
    if currentRound == roundAffected then
        lastRoundStatus

    else
        initRS


canAct : CrewmateStatus -> { a | currentRound : Int } -> Bool
canAct cs r =
    let
        { actions, ordered } =
            getCurrentRoundStatus cs r
    in
    actions == 0 || ordered && actions == 1


pilotCheckHelper_ : Bool -> CrewmateStatus -> { a | currentRound : Int } -> Maybe ( CrewmateStatus, Int )
pilotCheckHelper_ costsResolve cms ({ currentRound } as r) =
    if canAct cms r && (cms.resolvePoints > 0 || not costsResolve) then
        let
            currentRoundStatus =
                getCurrentRoundStatus cms r

            bonus =
                getPilotingSkillModifier cms r
        in
        Just
            ( { cms
                | roundStatus =
                    ( currentRound
                    , { currentRoundStatus | actions = currentRoundStatus.actions + 1 }
                    )
                , resolvePoints =
                    cms.resolvePoints
                        - (if costsResolve then
                            1

                           else
                            0
                          )
              }
            , bonus
            )

    else
        Nothing


pilotCheckHelper : CrewmateStatus -> { a | currentRound : Int } -> Maybe ( CrewmateStatus, Int )
pilotCheckHelper =
    pilotCheckHelper_ False


maneuver : CrewmateStatus -> { a | currentRound : Int } -> Maybe ( CrewmateStatus, Int )
maneuver =
    pilotCheckHelper


backOff : CrewmateStatus -> { a | currentRound : Int } -> Maybe ( CrewmateStatus, Int )
backOff =
    pilotCheckHelper


barrelRoll : CrewmateStatus -> { a | currentRound : Int } -> Maybe ( CrewmateStatus, Int )
barrelRoll =
    pilotCheckHelper


evade : CrewmateStatus -> { a | currentRound : Int } -> Maybe ( CrewmateStatus, Int )
evade =
    pilotCheckHelper


flipAndBurn : CrewmateStatus -> { a | currentRound : Int } -> Maybe ( CrewmateStatus, Int )
flipAndBurn =
    pilotCheckHelper


flyby : CrewmateStatus -> { a | currentRound : Int } -> Maybe ( CrewmateStatus, Int )
flyby =
    pilotCheckHelper


slide : CrewmateStatus -> { a | currentRound : Int } -> Maybe ( CrewmateStatus, Int )
slide =
    pilotCheckHelper


turnInPlace : CrewmateStatus -> { a | currentRound : Int } -> Maybe ( CrewmateStatus, Int )
turnInPlace =
    pilotCheckHelper


fullPower : CrewmateStatus -> { a | currentRound : Int } -> Maybe ( CrewmateStatus, Int )
fullPower =
    pilotCheckHelper_ True


audaciousGambit : CrewmateStatus -> { a | currentRound : Int } -> Maybe ( CrewmateStatus, Int )
audaciousGambit =
    pilotCheckHelper_ True


demandSource : CrewmateStatus -> { a | target : String, currentRound : Int } -> Maybe CrewmateStatus
demandSource cms ({ target, currentRound } as r) =
    if Set.member target cms.crewDemanded && canAct cms r then
        Nothing

    else
        let
            currentRoundStatus =
                getCurrentRoundStatus cms r
        in
        Just
            { cms
                | crewDemanded = Set.insert target cms.crewDemanded
                , roundStatus =
                    ( currentRound
                    , { currentRoundStatus | actions = currentRoundStatus.actions + 1 }
                    )
            }


demandTarget : CrewmateStatus -> { a | currentRound : Int } -> CrewmateStatus
demandTarget cms ({ currentRound } as r) =
    let
        currentRoundStatus =
            getCurrentRoundStatus cms r
    in
    { cms | roundStatus = ( currentRound, { currentRoundStatus | demanded = True } ) }


encourageSource : CrewmateStatus -> { a | currentRound : Int } -> Maybe CrewmateStatus
encourageSource cms ({ currentRound } as r) =
    if canAct cms r then
        let
            currentRoundStatus =
                getCurrentRoundStatus cms r
        in
        Just
            { cms
                | roundStatus =
                    ( currentRound
                    , { currentRoundStatus | actions = currentRoundStatus.actions + 1 }
                    )
            }

    else
        Nothing


encourageTarget : CrewmateStatus -> { a | currentRound : Int } -> CrewmateStatus
encourageTarget cms ({ currentRound } as r) =
    let
        currentRoundStatus =
            getCurrentRoundStatus cms r
    in
    { cms | roundStatus = ( currentRound, { currentRoundStatus | encouraged = True } ) }


tauntSource : CrewmateStatus -> { a | currentRound : Int } -> Maybe ( CrewmateStatus, ( Int, Int ) )
tauntSource cms ({ currentRound } as r) =
    if canAct cms r then
        let
            currentRoundStatus =
                getCurrentRoundStatus cms r
        in
        Just
            ( { cms
                | roundStatus =
                    ( currentRound
                    , { currentRoundStatus | actions = currentRoundStatus.actions + 1 }
                    )
              }
            , ( getIntimidateSkillModifier cms r, getBluffSkillModifier cms r )
            )

    else
        Nothing


ordersSource : CrewmateStatus -> { a | currentRound : Int } -> Maybe CrewmateStatus
ordersSource cms ({ currentRound } as r) =
    if cms.resolvePoints > 0 && canAct cms r then
        let
            currentRoundStatus =
                getCurrentRoundStatus cms r
        in
        Just
            { cms
                | resolvePoints = cms.resolvePoints - 1
                , roundStatus =
                    ( currentRound
                    , { currentRoundStatus | actions = currentRoundStatus.actions + 1 }
                    )
            }

    else
        Nothing


ordersTarget : CrewmateStatus -> { a | currentRound : Int } -> CrewmateStatus
ordersTarget cms ({ currentRound } as r) =
    let
        currentRoundStatus =
            getCurrentRoundStatus cms r
    in
    { cms | roundStatus = ( currentRound, { currentRoundStatus | ordered = True } ) }


movingSpeechSource : CrewmateStatus -> { a | currentRound : Int } -> Maybe ( CrewmateStatus, Int )
movingSpeechSource cms ({ currentRound } as r) =
    if cms.resolvePoints > 0 && canAct cms r then
        let
            currentRoundStatus =
                getCurrentRoundStatus cms r
        in
        Just
            ( { cms
                | resolvePoints = cms.resolvePoints - 1
                , roundStatus =
                    ( currentRound
                    , { currentRoundStatus | actions = currentRoundStatus.actions + 1 }
                    )
              }
            , getDiplomacySkillModifier cms r
            )

    else
        Nothing



-- TODO: Since we affect _all_ crew aboard, it may more be better to put this
-- on the Status (though it could be denormalized in our HTTP response).


movingSpeechTarget : CrewmateStatus -> { a | currentRound : Int, currentPhase : CombatPhase } -> CrewmateStatus
movingSpeechTarget cms ({ currentRound, currentPhase } as r) =
    let
        currentRoundStatus =
            getCurrentRoundStatus cms r

        ( phaseAffected, lastPhaseStatus ) =
            currentRoundStatus.phaseStatus

        currentPhaseStatus =
            if currentPhase == phaseAffected then
                lastPhaseStatus

            else
                initPS
    in
    { cms | roundStatus = ( currentRound, { currentRoundStatus | phaseStatus = ( currentPhase, { currentPhaseStatus | moved = True } ) } ) }
