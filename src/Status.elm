module Status exposing (ExtraPoweredSystem(..), Status, areShieldsFull, backOff, backOffFail, backOffFailBy5OrMore, balanceEvenly, balanceFromArc, barrelRoll, barrelRollFail, barrelRollFailBy5OrMore, basePatchDC, canBalanceFromTo, damageArc, damageSystem, divertPowerToEngines, divertPowerToShields, evade, evadeFailBy5OrMore, forceAddShields, forceMoveShields, getEffectiveAcAndTl, getEffectiveBonusOld, getEffectiveDistanceBetweenTurns, getEffectiveSpecialPilotResult, getEffectiveSpeed, hasExtraPower, holdItTogether, init, maneuver, maxDivertPowerToShieldPoints, movingSpeechSource, movingSpeechTarget, patch, quickFix)

import Arc exposing (AnArc, Arc)
import Assignments exposing (Assignments, allInEngineering)
import CombatPhase exposing (CombatPhase)
import Crewmate exposing (Crewmate)
import CrewmateStatus exposing (CrewmateStatus)
import CriticalStatus as CS exposing (CriticalStatus, PatchEffectiveness(..), Severity(..))
import DefenseLevel
import Dict exposing (Dict)
import PatchableSystems as PS exposing (PatchableSystem(..), PatchableSystems)
import PilotResult exposing (PilotResult, SpecialPilotResult, noPilotResult)
import Size exposing (Size(..))
import Starship exposing (Starship)
import Switch exposing (Switch(..))
import Togglable exposing (extract, meta)


type ExtraPoweredSystem
    = EngineSpeed
    | ScienceEquipment
    | Weapons
    | Shields


type PowerAction
    = Divert ExtraPoweredSystem
    | Overpower (List ExtraPoweredSystem) -- UUUUUUUUGH The Elm maintainers are so stupid sometimes.



-- TODO: There's mounting evidence that there are multiple status layers.
-- There's a Build, then a Starship with things that persist from battle to
-- battle (hulle damage, crew, critical systems) and then there are things that
-- only last during a battle or even a single round (pilot results, power
-- actions)


type Taunted
    = Push
    | NotPush


type alias Status =
    { damage : Int
    , shields : Arc Int
    , systems : PS.PatchableSystems (Maybe CriticalStatus)
    , powerAction : ( Int, PowerAction ) -- The most recent round's Power Action
    , pilotResult : ( Int, PilotResult ) -- TODO: While totally bizzare, if ordered, the pilot could theoretically do two stunts

    -- TODO: Ideally CrewId (or just an "assignable thing") isn't a String, but
    -- a parameterized type so that it can be any identifier, or a
    -- CrewmateStatus, or a (Crewmate, CrewmateStatus), or something else.
    -- However, this parameterization gets _everywhere_, so it's a bit of apain
    -- to do now.
    , crew : Dict String Crewmate

    -- TODO: It seems like two Dicts is a waste here.  This doesn't well
    -- capture our invariant (exactly one crewmate per status and vice versa),
    -- and it means we have to handle missing dict entries twice, which is just
    -- extra work.
    , crewStatus : Dict String CrewmateStatus

    -- TODO: There's really no way to possibly enforce all invariants
    -- simultaneously.  Perhaps an assignment is really just a single round
    -- status item, and this should just be collapsed into a single crew entry.
    , assignments : Assignments String
    , tauntedBy : Dict String ( Int, Taunted ) --TODO: affects one phase (over multiple rounds) unless it is a Push action
    }


init : Dict String Crewmate -> Status
init crew =
    { damage = 0
    , shields = Arc.pure 0
    , systems = PS.pure Nothing
    , powerAction = ( -1, Divert Shields ) -- The -1 round acts as a No-op
    , pilotResult = ( -1, noPilotResult ) -- The -1 round acts as a No-op
    , crew = crew
    , crewStatus = Dict.map (always (always CrewmateStatus.init)) crew
    , assignments = allInEngineering (Dict.keys crew)
    , tauntedBy = Dict.empty
    }


hasExtraPower : ExtraPoweredSystem -> Int -> Status -> Bool
hasExtraPower extraPoweredSystem roundNumber status =
    let
        ( effectiveRound, action ) =
            status.powerAction
    in
    if effectiveRound /= roundNumber then
        False

    else
        List.member extraPoweredSystem <|
            case action of
                Divert x ->
                    [ x ]

                Overpower s ->
                    s


getEffectiveBonus : Bool -> Int -> PatchableSystem -> Status -> Maybe Int
getEffectiveBonus isPush currentRound involvedSystem status =
    let
        bonusBySystem system =
            PS.getPatchableSystem system status.systems
                |> Maybe.andThen (CS.getEffectiveSeverity currentRound)

        standardBonus =
            case bonusBySystem involvedSystem of
                Nothing ->
                    Just 0

                Just Glitching ->
                    Just -2

                Just Malfunctioning ->
                    if isPush then
                        Nothing

                    else
                        Just -4

                Just Wrecked ->
                    Nothing

        powerCoreEffectBonus =
            case bonusBySystem PowerCore of
                Nothing ->
                    0

                Just Glitching ->
                    0

                Just Malfunctioning ->
                    -2

                Just Wrecked ->
                    -4

        tauntedByBonus =
            status.tauntedBy
                |> Dict.values
                |> List.map
                    (\( effectiveUntil, push ) ->
                        if currentRound <= effectiveUntil then
                            0

                        else
                            case push of
                                Push ->
                                    -4

                                NotPush ->
                                    -2
                    )
                |> List.sum

        effectivePowerCoreEffectBonus =
            case involvedSystem of
                PowerCore ->
                    0

                _ ->
                    powerCoreEffectBonus
    in
    Maybe.map
        (\sb -> sb + tauntedByBonus + effectivePowerCoreEffectBonus)
        standardBonus


getEffectiveBonusOld : Int -> PatchableSystem -> Status -> Int
getEffectiveBonusOld currentRound involvedSystem =
    getEffectiveBonus False currentRound involvedSystem >> Maybe.withDefault 0


canUse : Bool -> Int -> PatchableSystem -> Status -> Bool
canUse isPush currentRound involvedSystem status =
    let
        bonusBySystem system =
            PS.getPatchableSystem system status.systems
                |> Maybe.andThen (CS.getEffectiveSeverity currentRound)
    in
    case bonusBySystem involvedSystem of
        Just Malfunctioning ->
            not isPush

        Just Wrecked ->
            False

        _ ->
            True


basePatchDC : PatchEffectiveness -> PatchableSystem -> Status -> Maybe Int
basePatchDC pe ps s =
    Maybe.andThen (CS.basePatchDC pe) (PS.getPatchableSystem ps s.systems)


updateSystems : (PatchableSystems (Maybe CriticalStatus) -> PatchableSystems (Maybe CriticalStatus)) -> Status -> Status
updateSystems f status =
    { status | systems = f status.systems }


updateCriticalStatus : (Maybe CriticalStatus -> Maybe CriticalStatus) -> PatchableSystem -> Status -> Status
updateCriticalStatus f =
    PS.updatePatchableSystem f >> updateSystems


patch : PatchEffectiveness -> PatchableSystem -> Status -> Status
patch pe =
    updateCriticalStatus (Maybe.map (CS.patch pe))


holdItTogether : Int -> PatchableSystem -> Status -> Status
holdItTogether currentRound =
    updateCriticalStatus (Maybe.map (CS.holdItTogether currentRound))


quickFix : PatchableSystem -> Status -> Status
quickFix =
    updateCriticalStatus (Maybe.map CS.quickFix)


damageSystem : Maybe Int -> PatchableSystem -> Status -> Status
damageSystem mEmpUntilRound =
    updateCriticalStatus (CS.damage mEmpUntilRound >> Just)


damageArc : Bool -> Starship -> AnArc -> Int -> Status -> ( Int, Status )
damageArc wasCrit build arc amount status =
    let
        criticalThreshold =
            Starship.getMaxHitPoints build // 5

        shieldsOn =
            meta build.shields == On

        shielding =
            if shieldsOn then
                Arc.getArc arc status.shields

            else
                0

        hullDamage =
            amount - shielding
    in
    if hullDamage <= 0 then
        ( 0, { status | shields = Arc.updateArc (\x -> x - amount) arc status.shields } )

    else
        let
            oldCriticalThresholdCount =
                status.damage // criticalThreshold

            newCriticalThresholdCount =
                (status.damage + hullDamage) // criticalThreshold

            critCount =
                (newCriticalThresholdCount
                    - oldCriticalThresholdCount
                )
                    + (if wasCrit then
                        1

                       else
                        0
                      )
        in
        ( critCount
        , { status
            | shields = Arc.updateArc (always 0) arc status.shields
            , damage = status.damage + hullDamage
          }
        )


forceAddShields : Arc Int -> Status -> Status
forceAddShields new status =
    { status | shields = Arc.liftA2 (+) status.shields new }


balanceEvenly : Starship -> Status -> Status
balanceEvenly starship ({ shields } as status) =
    let
        total =
            Arc.sum shields

        quarter =
            total // 4

        leftover =
            total - 4 * quarter

        newShields =
            shields
                -- Add 1/4 to each
                |> Arc.map (always quarter)
                -- add the leftover to the forward arc
                |> Arc.setArc ((+) leftover) Arc.Forward
    in
    { status | shields = newShields }


maxDivertPowerToShieldPoints_ : Starship -> Arc Int -> Int
maxDivertPowerToShieldPoints_ starship shields =
    let
        maxProvidedByPowerCore =
            starship.powerCoreUnits // 20

        maxTotalShieldPoints =
            -- TODO: this ignores if shields are off
            (extract starship.shields).shieldPoints

        currentTotalShieldPoints =
            Arc.sum shields

        maxAddable =
            maxTotalShieldPoints - currentTotalShieldPoints
    in
    min maxProvidedByPowerCore maxAddable


maxDivertPowerToShieldPoints : Starship -> Status -> Int
maxDivertPowerToShieldPoints starship status =
    maxDivertPowerToShieldPoints_ starship status.shields


divertPowerToShields_ : Starship -> Arc Int -> Arc Int -> Maybe (Arc Int)
divertPowerToShields_ starship added shields =
    let
        allPositive =
            added
                |> Arc.map (\x -> x >= 0)
                |> Arc.all

        pointsAdded =
            Arc.sum added

        maxAddable =
            maxDivertPowerToShieldPoints_ starship shields
    in
    if not allPositive || pointsAdded /= maxAddable then
        Nothing

    else
        Just <| Arc.liftA2 (+) added shields


divertPowerToShields : Starship -> Arc Int -> Int -> Status -> Maybe Status
divertPowerToShields starship added currentRound status =
    Maybe.map
        (\shields -> { status | shields = shields, powerAction = ( currentRound, Divert Shields ) })
        (divertPowerToShields_ starship added status.shields)


divertPowerToEngines : Starship -> Int -> Status -> Status
divertPowerToEngines starship currentRound status =
    { status | powerAction = ( currentRound, Divert EngineSpeed ) }


areShieldsFull : Starship -> Status -> Bool
areShieldsFull starship status =
    -- TODO: this ignores if shields are off
    Arc.sum status.shields >= (extract starship.shields).shieldPoints



-- Given an arc, we check which arcs it can send shield points to via the
-- balance action.  At the end of balancing, all arcs must contain at least 10%
-- of the current total, which means there are three possiblities: we can
-- balance to any arc, we can balance to exactly one arc in need, or we cannot
-- balance to any.  In the latter case it may be because the from arc is less
-- than 10%, because it doesn't have enough to give to put a to arc at or above
-- 10%, or if there are two or more arcs below 10% we can't satisfy the 10%
-- minimum rule.  This rule is dumb.


canBalanceFromTo : AnArc -> Arc Int -> List AnArc
canBalanceFromTo from shields =
    let
        tenPercent =
            Arc.sum shields // 10

        -- We determine how many points we can share from the from arc, and
        -- then which arcs are in need of points to get to 10% (and the number
        -- of points required to do so)
        ( sharablePoints, quadrantsInNeed ) =
            Arc.foldWithAnArc
                (\arc excess ( sharablePoints_, quadrantsInNeed_ ) ->
                    if arc == from then
                        ( excess, quadrantsInNeed_ )

                    else
                        ( sharablePoints_
                        , (if excess < 0 then
                            (::) ( arc, excess )

                           else
                            identity
                          )
                            quadrantsInNeed_
                        )
                )
                ( 0, [] )
                (Arc.map (\x -> x - tenPercent) shields)
    in
    case ( quadrantsInNeed, sharablePoints > 0 ) of
        -- If the from quadrant exceeds 10% and all other quadrants are not
        -- below, then we can balance to any arc
        ( [], True ) ->
            List.filter ((/=) from)
                [ Arc.Forward
                , Arc.Aft
                , Arc.Port
                , Arc.Starboard
                ]

        -- If the from quadrant exceeds 10% and there is exactly one quadrant
        -- that is below 10% _and_ the from quadrant has sufficient points to
        -- push it beyond 10%, then we can balance to that quadrant (and _only_
        -- that quadrant)
        ( [ ( needArc, needExcess ) ], True ) ->
            if sharablePoints >= needExcess then
                [ needArc ]

            else
                []

        -- In any other case, we cannot balance from this arc at all
        _ ->
            []


forceMoveShields : ( AnArc, AnArc, Int ) -> Status -> Status
forceMoveShields ( from, to, amount ) status =
    let
        asArc =
            Arc.pureWithAnArc
                (\arc ->
                    if arc == from then
                        -amount

                    else if arc == to then
                        amount

                    else
                        0
                )
    in
    forceAddShields asArc status


balanceFromArc : Starship -> ( AnArc, AnArc, Int ) -> Status -> Maybe Status
balanceFromArc starship ( from, to, amount ) status =
    let
        tenPercent =
            Arc.sum status.shields // 10

        afterBalance =
            forceMoveShields ( from, to, amount ) status

        validFromTo =
            List.member to (canBalanceFromTo from status.shields)

        allAboveTenPercent =
            afterBalance.shields
                |> Arc.map ((<=) tenPercent)
                |> Arc.all
    in
    if validFromTo && allAboveTenPercent then
        Just afterBalance

    else
        Nothing


applyPilotResult : (Starship -> PilotResult) -> Starship -> Int -> Status -> Status
applyPilotResult f starship currentRound status =
    { status | pilotResult = ( currentRound, f starship ) }


pilotCheckHelper : PilotResult -> (Crewmate -> Int) -> (CrewmateStatus -> { a | currentRound : Int } -> Maybe ( CrewmateStatus, Int )) -> Status -> { a | currentRound : Int } -> Maybe ( Status, Int )
pilotCheckHelper pr f g status ({ currentRound } as r) =
    let
        mPilot =
            status.assignments.pilot

        mCrewBonus =
            mPilot
                |> Maybe.andThen (\p -> Dict.get p status.crew)
                |> Maybe.map f

        mNewCrewStatusAndBonus =
            mPilot
                |> Maybe.andThen
                    (\p ->
                        Dict.get p status.crewStatus
                            |> Maybe.andThen (\x -> g x r)
                            |> Maybe.map (\( s, b ) -> ( Dict.insert p s status.crewStatus, b ))
                    )

        mNonCrewBonus =
            getEffectiveBonus False currentRound Engines status
    in
    case ( ( mCrewBonus, mNewCrewStatusAndBonus ), mNonCrewBonus ) of
        ( ( Just crewBonus, Just ( newCrewStatus, crewStatusBonus ) ), Just nonCrewBonus ) ->
            Just
                ( { status
                    | pilotResult = ( currentRound, pr )
                    , crewStatus = newCrewStatus
                  }
                , crewBonus + crewStatusBonus + nonCrewBonus
                )

        _ ->
            Nothing


maneuver : Status -> { a | currentRound : Int } -> Maybe ( Status, Int )
maneuver =
    pilotCheckHelper PilotResult.maneuver Crewmate.maneuver CrewmateStatus.maneuver


backOff : Status -> { a | starship : Starship, currentRound : Int } -> Maybe ( Status, Int )
backOff status ({ starship } as r) =
    pilotCheckHelper (PilotResult.backOff starship) Crewmate.backOff CrewmateStatus.backOff status r



-- TODO: This is probably one of the missing pieces from our emerging "action
-- Monad" (MonadReader a + MonadState Thing + MonadWriter Bonus), where really
-- there's a range of effects that could occur.  Not just the
-- list/nondeterminism monad, because these usually relate to the DC (but there
-- _can_ be multiple nondeterministic things, I suppose).  It seems basically
-- impossible to combine/collapse these together though.


backOffFail : Status -> { a | currentRound : Int } -> Maybe ( Status, Int )
backOffFail =
    pilotCheckHelper PilotResult.backOffFail Crewmate.backOff CrewmateStatus.backOff


backOffFailBy5OrMore : Status -> { a | starship : Starship, currentRound : Int } -> Maybe ( Status, Int )
backOffFailBy5OrMore status ({ starship } as r) =
    pilotCheckHelper (PilotResult.backOffFailBy5OrMore starship) Crewmate.backOff CrewmateStatus.backOff status r


canBarrelRoll : Starship -> Bool
canBarrelRoll starship =
    case starship.frame.size of
        Colossal ->
            False

        Gargantuan ->
            False

        Huge ->
            False

        _ ->
            True


barrelRoll : Status -> { a | starship : Starship, currentRound : Int } -> Maybe ( Status, Int )
barrelRoll status ({ starship } as r) =
    if canBarrelRoll starship then
        pilotCheckHelper (PilotResult.barrelRoll starship) Crewmate.barrelRoll CrewmateStatus.barrelRoll status r

    else
        Nothing


barrelRollFail : Status -> { a | starship : Starship, currentRound : Int } -> Maybe ( Status, Int )
barrelRollFail status ({ starship } as r) =
    if canBarrelRoll starship then
        pilotCheckHelper (PilotResult.barrelRollFail starship) Crewmate.barrelRoll CrewmateStatus.barrelRoll status r

    else
        Nothing


barrelRollFailBy5OrMore : Status -> { a | starship : Starship, currentRound : Int } -> Maybe ( Status, Int )
barrelRollFailBy5OrMore status ({ starship } as r) =
    if canBarrelRoll starship then
        pilotCheckHelper (PilotResult.barrelRollFailBy5OrMore starship) Crewmate.barrelRoll CrewmateStatus.barrelRoll status r

    else
        Nothing


evade : Status -> { a | currentRound : Int } -> Maybe ( Status, Int )
evade =
    pilotCheckHelper PilotResult.evade Crewmate.evade CrewmateStatus.evade


evadeFailBy5OrMore : Status -> { a | currentRound : Int } -> Maybe ( Status, Int )
evadeFailBy5OrMore =
    pilotCheckHelper PilotResult.evadeFailBy5OrMore Crewmate.evade CrewmateStatus.evade


flipAndBurn : Starship -> Int -> Status -> Status
flipAndBurn =
    applyPilotResult PilotResult.flipAndBurn


flipAndBurnFail : Starship -> Int -> Status -> Status
flipAndBurnFail =
    applyPilotResult PilotResult.flipAndBurnFail


flyby : Starship -> Int -> Status -> Status
flyby =
    applyPilotResult (always PilotResult.flyby)


flybyFail : Starship -> Int -> Status -> Status
flybyFail =
    applyPilotResult (always PilotResult.flybyFail)


slide : Starship -> Int -> Status -> Status
slide =
    applyPilotResult (always PilotResult.slide)


slideFail : Starship -> Int -> Status -> Status
slideFail =
    applyPilotResult PilotResult.slideFail


turnInPlace : Starship -> Int -> Status -> Status
turnInPlace =
    applyPilotResult PilotResult.turnInPlace


fullPower : Starship -> Int -> Status -> Status
fullPower =
    -- TODO: Also costs a Resolve Point
    applyPilotResult PilotResult.fullPower


audaciousGambit : Starship -> Int -> Status -> Status
audaciousGambit =
    applyPilotResult (always PilotResult.audaciousGambit)


audaciousGambitFail : Starship -> Int -> Status -> Status
audaciousGambitFail =
    -- TODO: Also costs a Resolve Point
    -- Just updates the most recent pilot effect (but has none)
    applyPilotResult (always PilotResult.audaciousGambitFail)


getEffectiveAcAndTl : Starship -> Int -> Status -> ( Int, Int )
getEffectiveAcAndTl starship currentRound status =
    let
        pilot =
            status.assignments.pilot
                |> Maybe.andThen (\id -> Dict.get id status.crew)

        pilotAcBonus =
            pilot
                |> Maybe.map Crewmate.getAcModifier
                |> Maybe.withDefault 0

        pilotTlBonus =
            pilot
                |> Maybe.map Crewmate.getTlModifier
                |> Maybe.withDefault 0

        ( pilotResultRound, pilotResult ) =
            status.pilotResult

        ( pilotResultAcBonus, pilotResultTlBonus ) =
            if currentRound == pilotResultRound then
                ( PilotResult.getAcModifier pilotResult
                , PilotResult.getTlModifier pilotResult
                )

            else
                ( 0, 0 )

        baseValue =
            10
    in
    ( baseValue + pilotAcBonus + pilotResultAcBonus + Starship.getAcModifier starship
    , baseValue + pilotTlBonus + pilotResultTlBonus + Starship.getTlModifier starship
    )


getEffectiveSpeed : Starship -> Int -> Status -> Int
getEffectiveSpeed starship currentRound status =
    -- TODO: Do we multiply from stunts or add from engineer actions first?
    let
        engineeringBonus =
            if hasExtraPower EngineSpeed currentRound status then
                2

            else
                0

        ( pilotResultRound, pilotResult ) =
            status.pilotResult

        pilotBonus =
            if currentRound == pilotResultRound then
                PilotResult.getSpeedModifier pilotResult

            else
                0
    in
    Starship.getSpeedModifier starship + engineeringBonus + pilotBonus


getEffectiveDistanceBetweenTurns : Starship -> Int -> Status -> Int
getEffectiveDistanceBetweenTurns starship currentRound status =
    let
        starshipModifier =
            Starship.getDistanceBetweenTurnsModifier starship

        ( pilotResultRound, pilotResult ) =
            status.pilotResult

        pilotEffect =
            if currentRound == pilotResultRound then
                PilotResult.getDistanceBetweenTurnsModifier pilotResult

            else
                0
    in
    max 0 (starshipModifier + pilotEffect)


getEffectiveSpecialPilotResult : Status -> { a | currentRound : Int } -> Maybe SpecialPilotResult
getEffectiveSpecialPilotResult { pilotResult } { currentRound } =
    if Tuple.first pilotResult == currentRound then
        (Tuple.second pilotResult).special

    else
        Nothing



-- TODO: Sometimes bonuses reduce DC--but mathematically this is the same
-- thing.  Perhaps a Bonus could be a complex type that indicates if it
-- modifies the roll or the DC.
--
-- TODO: It may also make sense to model "take best roll of two" or "take worst
-- roll of two" as a bonus as well.
--
-- TODO: Computer nodes can be consumed to gain additional bonuses
--
-- TODO: Need to determine if the check _can_ be peformed (qualified, done
-- before, too damaged, sufficient RP, etc)
-- TODO: This module shouldn't really need to know about "skills," instead
-- bonuses should be abstracted.  Ex. Crewmate.blankSource should return the
-- bonus (which may be using getBlankSkillModifier under the hood.


getXSkillModifier : (Crewmate -> Int) -> (CrewmateStatus -> r -> Int) -> ( String, Status ) -> r -> Int
getXSkillModifier f g ( crewId, status ) r =
    let
        cMod =
            Dict.get crewId status.crew
                |> Maybe.map f
                |> Maybe.withDefault 0

        csMod =
            Dict.get crewId status.crewStatus
                |> Maybe.map (\x -> g x r)
                |> Maybe.withDefault 0
    in
    cMod + csMod


getIntimidateSkillModifier : ( String, Status ) -> { a | currentRound : Int } -> Int
getIntimidateSkillModifier =
    getXSkillModifier
        Crewmate.getIntimidateSkillModifier
        CrewmateStatus.getIntimidateSkillModifier


getBluffSkillModifier : ( String, Status ) -> { a | currentRound : Int } -> Int
getBluffSkillModifier =
    getXSkillModifier
        Crewmate.getBluffSkillModifier
        CrewmateStatus.getBluffSkillModifier


getEngineeringSkillModifier : ( String, Status ) -> { a | currentRound : Int } -> Int
getEngineeringSkillModifier =
    getXSkillModifier
        Crewmate.getEngineeringSkillModifier
        CrewmateStatus.getEngineeringSkillModifier


getGunningModifier : ( String, Status ) -> { a | currentRound : Int } -> Int
getGunningModifier =
    getXSkillModifier
        Crewmate.getGunningModifier
        CrewmateStatus.getGunningModifier


getPilotingSkillModifier : ( String, Status ) -> { a | currentRound : Int } -> Int
getPilotingSkillModifier =
    getXSkillModifier
        Crewmate.getPilotingSkillModifier
        CrewmateStatus.getPilotingSkillModifier


getComputersSkillModifier : ( Int, String, Status ) -> { a | currentRound : Int } -> Int
getComputersSkillModifier ( currentRound, crewId, status ) r =
    let
        base =
            getXSkillModifier
                Crewmate.getComputersSkillModifier
                CrewmateStatus.getComputersSkillModifier
                ( crewId, status )
                r

        dMod =
            if hasExtraPower ScienceEquipment currentRound status then
                2

            else
                0
    in
    base + dMod


demandSource : Status -> { a | currentRound : Int, target : String } -> Maybe ( Status, Int )
demandSource status ({ currentRound, target } as reader) =
    let
        updateCaptain captain =
            Dict.get captain status.crewStatus
                |> Maybe.andThen
                    (\ccs -> CrewmateStatus.demandSource ccs reader)
                |> Maybe.map
                    (\ccs ->
                        { status
                            | crewStatus =
                                Dict.insert
                                    captain
                                    ccs
                                    status.crewStatus
                        }
                    )
    in
    Maybe.map2 (\a b -> ( a, b ))
        (Maybe.andThen updateCaptain status.assignments.captain)
        (getEffectiveBonus False currentRound LifeSupport status)


demandTarget : Status -> { a | currentRound : Int, target : String } -> Maybe ( Status, Int )
demandTarget status ({ currentRound, target } as r) =
    Dict.get target status.crewStatus
        |> Maybe.map
            (\tcs ->
                let
                    cs =
                        Dict.insert
                            target
                            (CrewmateStatus.demandTarget tcs r)
                            status.crewStatus
                in
                ( { status | crewStatus = cs }, 0 )
            )



-- TODO: Encourage is interesting, because you can essentially get a +5 (DC
-- reduced by 5) for using the associated skill instead of diplomacy


encourageSource : Status -> { a | currentRound : Int, target : String } -> Maybe Status
encourageSource status ({ currentRound, target } as reader) =
    let
        updateCaptain captain =
            Dict.get captain status.crewStatus
                |> Maybe.andThen (\x -> CrewmateStatus.encourageSource x reader)
                |> Maybe.map
                    (\ccs ->
                        { status
                            | crewStatus =
                                Dict.insert
                                    captain
                                    ccs
                                    status.crewStatus
                        }
                    )
    in
    if canUse False currentRound LifeSupport status then
        status.assignments.captain
            |> Maybe.andThen updateCaptain

    else
        Nothing


encourageTarget : Status -> { a | currentRound : Int, target : String } -> Maybe Status
encourageTarget status ({ currentRound, target } as r) =
    Dict.get target status.crewStatus
        |> Maybe.map
            (\tcs ->
                let
                    cs =
                        Dict.insert
                            target
                            (CrewmateStatus.encourageTarget tcs r)
                            status.crewStatus
                in
                { status | crewStatus = cs }
            )


tauntSource : Status -> { a | currentRound : Int } -> Maybe ( Status, Int )
tauntSource status ({ currentRound } as r) =
    let
        updateCaptain captain =
            Dict.get captain status.crewStatus
                |> Maybe.andThen (\x -> CrewmateStatus.tauntSource x r)
                |> Maybe.map (\cs -> Dict.insert captain cs status.crewStatus)
                |> Maybe.map (\cs -> { status | crewStatus = cs })

        captainBonus captain =
            max
                (getBluffSkillModifier ( captain, status ) r)
                (getIntimidateSkillModifier ( captain, status ) r)

        captainResult =
            Maybe.map2 (\a b -> ( a, b ))
                (Maybe.andThen updateCaptain status.assignments.captain)
                (Maybe.map captainBonus status.assignments.captain)
    in
    Maybe.map2 (\( s, b ) b2 -> ( s, b + b2 ))
        captainResult
        (getEffectiveBonus True currentRound LifeSupport status)


tauntTarget : Status -> { a | source : String, currentRound : Int, taunted : Taunted } -> Maybe ( Status, Int )
tauntTarget status { source, currentRound, taunted } =
    if Dict.member source status.tauntedBy then
        Nothing

    else
        Just ( { status | tauntedBy = Dict.insert source ( currentRound, taunted ) status.tauntedBy }, 0 )



-- TODO: Orders is interesting, because you need to use the skill that target
-- character will use


ordersSource : Status -> { a | currentRound : Int } -> Maybe Status
ordersSource status ({ currentRound } as r) =
    let
        updateCrew captain =
            Dict.get captain status.crew
                |> Maybe.andThen Crewmate.ordersSource
                |> Maybe.map (\cm -> Dict.insert captain cm status.crew)

        updateCrewStatus captain =
            Dict.get captain status.crewStatus
                |> Maybe.andThen (\x -> CrewmateStatus.ordersSource x r)
                |> Maybe.map (\cs -> Dict.insert captain cs status.crewStatus)

        mNewCrew =
            status.assignments.captain
                |> Maybe.andThen updateCrew

        mNewCrewStatus =
            status.assignments.captain
                |> Maybe.andThen updateCrewStatus
    in
    if canUse True currentRound LifeSupport status then
        Maybe.map2
            (\c cs ->
                { status
                    | crewStatus = cs
                    , crew = c
                }
            )
            mNewCrew
            mNewCrewStatus

    else
        Nothing



-- TODO: This only affects a single crewmate (and the captain can't order
-- himself)


ordersTarget : Status -> { a | currentRound : Int } -> Status
ordersTarget status r =
    { status | crewStatus = Dict.map (always (\x -> CrewmateStatus.ordersTarget x r)) status.crewStatus }



-- TODO: This is similar to orders, like _really_ similar, but the true
-- abstraction just doesn't quite seem obvious


movingSpeechSource : Status -> { a | currentRound : Int } -> Maybe ( Status, Int )
movingSpeechSource status ({ currentRound } as r) =
    let
        updateCrewStatusAndGetBonus captain =
            Dict.get captain status.crewStatus
                |> Maybe.andThen (\x -> CrewmateStatus.movingSpeechSource x r)
                |> Maybe.map (\( cs, b ) -> ( Dict.insert captain cs status.crewStatus, b ))

        mCrewBonus =
            status.assignments.captain
                |> Maybe.andThen (\c -> Dict.get c status.crew)
                |> Maybe.andThen Crewmate.movingSpeechSource

        mNewCrewStatusAndBonus =
            status.assignments.captain
                |> Maybe.andThen updateCrewStatusAndGetBonus

        newState =
            Maybe.map
                (\( cs, _ ) -> { status | crewStatus = cs })
                mNewCrewStatusAndBonus

        mNonCrewBonus =
            getEffectiveBonus True currentRound LifeSupport status

        bonus =
            Maybe.map3 (\a ( _, b ) c -> a + b + c) mCrewBonus mNewCrewStatusAndBonus mNonCrewBonus
    in
    Maybe.map2 (\a b -> ( a, b )) newState bonus


movingSpeechTarget : Status -> { a | currentRound : Int, currentPhase : CombatPhase } -> ( Status, Int )
movingSpeechTarget status r =
    ( { status | crewStatus = Dict.map (always (\x -> CrewmateStatus.movingSpeechTarget x r)) status.crewStatus }, 0 )


divertBonus : ( Int, String, Status ) -> { a | currentRound : Int } -> Int
divertBonus ( currentRound, crewId, status ) r =
    getEngineeringSkillModifier ( crewId, status ) r
        + getEffectiveBonusOld currentRound PowerCore status


holdItTogetherBonus : ( Int, String, Status ) -> { a | currentRound : Int } -> Int
holdItTogetherBonus ( _, crewId, status ) =
    getEngineeringSkillModifier ( crewId, status )


patchBonus : ( Int, String, Status ) -> { a | currentRound : Int } -> Int
patchBonus ( _, crewId, status ) =
    getEngineeringSkillModifier ( crewId, status )


overpowerBonus : ( Int, String, Status ) -> { a | currentRound : Int } -> Int
overpowerBonus ( currentRound, crewId, status ) r =
    getEngineeringSkillModifier ( crewId, status ) r
        + getEffectiveBonusOld currentRound PowerCore status


quickFixBonus : ( Int, String, Status ) -> { a | currentRound : Int } -> Int
quickFixBonus ( currentRound, crewId, status ) r =
    getEngineeringSkillModifier ( crewId, status ) r
        + getEffectiveBonusOld currentRound PowerCore status



-- TODO: The science officer's "lock-on" grants a +2 bonus to the enemy target
-- TODO: Range penalties are unique by both weapon and distance
-- TODO: Turrets are affect by ALL Arcs


fireAtWillBonus : ( Int, ( String, AnArc ), Status ) -> { a | currentRound : Int } -> Int
fireAtWillBonus ( currentRound, ( crewId, arc ), status ) r =
    getEffectiveBonusOld currentRound (WeaponsArray arc) status
        + getGunningModifier ( crewId, status ) r
        - 4


shootBonus : ( Int, ( String, AnArc ), Status ) -> { a | currentRound : Int } -> Int
shootBonus ( currentRound, ( crewId, arc ), status ) r =
    getEffectiveBonusOld currentRound (WeaponsArray arc) status
        + getGunningModifier ( crewId, status ) r


broadsideBonus : ( Int, ( String, AnArc ), Status ) -> { a | currentRound : Int } -> Int
broadsideBonus ( currentRound, ( crewId, arc ), status ) r =
    getEffectiveBonusOld currentRound (WeaponsArray arc) status
        + getGunningModifier ( crewId, status ) r
        - 2


preciseTargetingBonus : ( Int, ( String, AnArc ), Status ) -> { a | currentRound : Int } -> Int
preciseTargetingBonus ( currentRound, ( crewId, arc ), status ) r =
    getEffectiveBonusOld currentRound (WeaponsArray arc) status
        + getGunningModifier ( crewId, status ) r


stuntBonus : ( Int, String, Status ) -> { a | currentRound : Int } -> Int
stuntBonus ( currentRound, crewId, status ) r =
    getPilotingSkillModifier ( crewId, status ) r
        + getEffectiveBonusOld currentRound Engines status


fullPowerBonus : ( Int, String, Status ) -> { a | currentRound : Int } -> Int
fullPowerBonus ( currentRound, crewId, status ) r =
    getPilotingSkillModifier ( crewId, status ) r
        + getEffectiveBonusOld currentRound Engines status


audaciousGambitBonus : ( Int, String, Status ) -> { a | currentRound : Int } -> Int
audaciousGambitBonus ( currentRound, crewId, status ) r =
    getPilotingSkillModifier ( crewId, status ) r
        + getEffectiveBonusOld currentRound Engines status


balanceBonus : ( Int, String, Status ) -> { a | currentRound : Int } -> Int
balanceBonus ( currentRound, crewId, status ) r =
    getComputersSkillModifier ( currentRound, crewId, status ) r
        + getEffectiveBonusOld currentRound Sensors status


scanBonus : ( Int, String, Status ) -> { a | currentRound : Int } -> Int
scanBonus ( currentRound, crewId, status ) r =
    getComputersSkillModifier ( currentRound, crewId, status ) r
        + getEffectiveBonusOld currentRound Sensors status


targetSystemBonus : ( Int, String, Status ) -> { a | currentRound : Int } -> Int
targetSystemBonus ( currentRound, crewId, status ) r =
    getComputersSkillModifier ( currentRound, crewId, status ) r
        + getEffectiveBonusOld currentRound Sensors status


lockOnBonus : ( Int, String, Status ) -> { a | currentRound : Int } -> Int
lockOnBonus ( currentRound, crewId, status ) r =
    getComputersSkillModifier ( currentRound, crewId, status ) r
        + getEffectiveBonusOld currentRound Sensors status


improveCountermeasuresBonus : ( Int, String, Status ) -> { a | currentRound : Int } -> Int
improveCountermeasuresBonus ( currentRound, crewId, status ) r =
    getComputersSkillModifier ( currentRound, crewId, status ) r
        + getEffectiveBonusOld currentRound Sensors status
