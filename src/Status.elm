module Status exposing (ExtraPoweredSystem(..), Status, areShieldsFull, audaciousGambit, audaciousGambitFail, backOff, backOffFail, backOffFailBy5OrMore, balanceEvenly, balanceFromArc, barrelRoll, barrelRollFail, barrelRollFailBy5OrMore, basePatchDC, canBalanceFromTo, damageArc, damageSystem, divertPowerToEngines, divertPowerToShields, evade, evadeFailBy5OrMore, flipAndBurn, flipAndBurnFail, flyby, flybyFail, forceAddShields, forceMoveShields, fullPower, getEffectiveAcAndTl, getEffectiveBonusOld, getEffectiveDistanceBetweenTurns, getEffectiveSpecialPilotResult, getEffectiveSpeed, hasExtraPower, holdItTogether, init, maneuver, maxDivertPowerToShieldPoints, movingSpeechSource, movingSpeechTarget, patch, quickFix, slide, slideFail, turnInPlace)

import Arc exposing (AnArc, Arc)
import Assignments exposing (Assignments, allInEngineering)
import CombatCrew exposing (CombatCrew)
import CombatPhase exposing (CombatPhase)
import Computer exposing (Computer)
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
    , computerNodesUsed : ( Int, Int )

    -- TODO: Ideally CrewId (or just an "assignable thing") isn't a String, but
    -- a parameterized type so that it can be any identifier, or a
    -- CrewmateStatus, or a (Crewmate, CrewmateStatus), or something else.
    -- However, this parameterization gets _everywhere_, so it's a bit of apain
    -- to do now.
    , crew : CombatCrew String
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
    , computerNodesUsed = ( -1, 0 ) -- The -1 round acts as a No-op
    , crew = Dict.map (\_ cm -> ( cm, CrewmateStatus.init )) crew
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


getComputerNodeBonus : ( Int, Int ) -> { a | useComputerNode : Bool, starship : Starship, currentRound : Int } -> Maybe ( ( Int, Int ), Int )
getComputerNodeBonus (( effectiveRound, usedNodeCount ) as s) { useComputerNode, starship, currentRound } =
    if useComputerNode then
        let
            computer =
                starship.computer

            newUsedNodeCount =
                if effectiveRound == currentRound then
                    usedNodeCount + 1

                else
                    1

            totalAvailable =
                if meta computer == On then
                    (extract computer).nodes

                else
                    0
        in
        if newUsedNodeCount <= totalAvailable then
            Just ( ( currentRound, newUsedNodeCount ), (extract computer).bonus )

        else
            Nothing

    else
        Just ( s, 0 )


getEffectiveBonusOld : Int -> PatchableSystem -> Status -> Int
getEffectiveBonusOld currentRound involvedSystem =
    getEffectiveBonus False currentRound involvedSystem >> Maybe.withDefault 0


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


pilotCheckHelper : PilotResult -> (CombatCrew String -> { currentRound : Int, assignments : Assignments String } -> Maybe ( CombatCrew String, Int )) -> Status -> { a | currentRound : Int, useComputerNode : Bool, starship : Starship } -> Maybe ( Status, Int )
pilotCheckHelper pr f status ({ currentRound } as r) =
    let
        mNewCrewAndBonus =
            f status.crew { currentRound = currentRound, assignments = status.assignments }

        mNonCrewBonus =
            getEffectiveBonus False currentRound Engines status

        mComputerNodeBonus =
            getComputerNodeBonus status.computerNodesUsed r
    in
    Maybe.map3
        (\( newCrew, crewBonus ) nonCrewBonus ( newComputerNodesUsed, computerBonus ) ->
            ( { status
                | pilotResult = ( currentRound, pr )
                , crew = newCrew
                , computerNodesUsed = newComputerNodesUsed
              }
            , crewBonus + nonCrewBonus + computerBonus
            )
        )
        mNewCrewAndBonus
        mNonCrewBonus
        mComputerNodeBonus


maneuver : Status -> { a | currentRound : Int, starship : Starship, useComputerNode : Bool } -> Maybe ( Status, Int )
maneuver =
    pilotCheckHelper PilotResult.maneuver CombatCrew.maneuver


backOff : Status -> { a | starship : Starship, currentRound : Int, useComputerNode : Bool } -> Maybe ( Status, Int )
backOff status ({ starship } as r) =
    pilotCheckHelper (PilotResult.backOff starship) CombatCrew.backOff status r



-- TODO: This is probably one of the missing pieces from our emerging "action
-- Monad" (MonadReader a + MonadState Thing + MonadWriter Bonus), where really
-- there's a range of effects that could occur.  Not just the
-- list/nondeterminism monad, because these usually relate to the DC (but there
-- _can_ be multiple nondeterministic things, I suppose).  It seems basically
-- impossible to combine/collapse these together though.


backOffFail : Status -> { a | currentRound : Int, starship : Starship, useComputerNode : Bool } -> Maybe ( Status, Int )
backOffFail =
    pilotCheckHelper PilotResult.backOffFail CombatCrew.backOff


backOffFailBy5OrMore : Status -> { a | starship : Starship, currentRound : Int, useComputerNode : Bool } -> Maybe ( Status, Int )
backOffFailBy5OrMore status ({ starship } as r) =
    pilotCheckHelper (PilotResult.backOffFailBy5OrMore starship) CombatCrew.backOff status r


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


barrelRoll : Status -> { a | starship : Starship, currentRound : Int, useComputerNode : Bool } -> Maybe ( Status, Int )
barrelRoll status ({ starship } as r) =
    if canBarrelRoll starship then
        pilotCheckHelper (PilotResult.barrelRoll starship) CombatCrew.barrelRoll status r

    else
        Nothing


barrelRollFail : Status -> { a | starship : Starship, currentRound : Int, useComputerNode : Bool } -> Maybe ( Status, Int )
barrelRollFail status ({ starship } as r) =
    if canBarrelRoll starship then
        pilotCheckHelper (PilotResult.barrelRollFail starship) CombatCrew.barrelRoll status r

    else
        Nothing


barrelRollFailBy5OrMore : Status -> { a | starship : Starship, currentRound : Int, useComputerNode : Bool } -> Maybe ( Status, Int )
barrelRollFailBy5OrMore status ({ starship } as r) =
    if canBarrelRoll starship then
        pilotCheckHelper (PilotResult.barrelRollFailBy5OrMore starship) CombatCrew.barrelRoll status r

    else
        Nothing


evade : Status -> { a | currentRound : Int, starship : Starship, useComputerNode : Bool } -> Maybe ( Status, Int )
evade =
    pilotCheckHelper PilotResult.evade CombatCrew.evade


evadeFailBy5OrMore : Status -> { a | currentRound : Int, starship : Starship, useComputerNode : Bool } -> Maybe ( Status, Int )
evadeFailBy5OrMore =
    pilotCheckHelper PilotResult.evadeFailBy5OrMore CombatCrew.evade


flipAndBurn : Status -> { a | starship : Starship, currentRound : Int, useComputerNode : Bool } -> Maybe ( Status, Int )
flipAndBurn status ({ starship } as r) =
    pilotCheckHelper (PilotResult.flipAndBurn starship) CombatCrew.flipAndBurn status r


flipAndBurnFail : Status -> { a | starship : Starship, currentRound : Int, useComputerNode : Bool } -> Maybe ( Status, Int )
flipAndBurnFail status ({ starship } as r) =
    pilotCheckHelper (PilotResult.flipAndBurnFail starship) CombatCrew.flipAndBurn status r


flyby : Status -> { a | currentRound : Int, starship : Starship, useComputerNode : Bool } -> Maybe ( Status, Int )
flyby =
    pilotCheckHelper PilotResult.flyby CombatCrew.flyby


flybyFail : Status -> { a | currentRound : Int, starship : Starship, useComputerNode : Bool } -> Maybe ( Status, Int )
flybyFail =
    pilotCheckHelper PilotResult.flybyFail CombatCrew.flyby


slide : Status -> { a | currentRound : Int, starship : Starship, useComputerNode : Bool } -> Maybe ( Status, Int )
slide =
    pilotCheckHelper PilotResult.slide CombatCrew.slide


slideFail : Status -> { a | starship : Starship, currentRound : Int, useComputerNode : Bool } -> Maybe ( Status, Int )
slideFail status ({ starship } as r) =
    pilotCheckHelper (PilotResult.slideFail starship) CombatCrew.slide status r



-- TODO: This doesn't require a skill check


turnInPlace : Status -> { a | starship : Starship, currentRound : Int, useComputerNode : Bool } -> Maybe ( Status, Int )
turnInPlace status ({ starship } as r) =
    pilotCheckHelper (PilotResult.turnInPlace starship) CombatCrew.turnInPlace status r


fullPower : Status -> { a | starship : Starship, currentRound : Int, useComputerNode : Bool } -> Maybe ( Status, Int )
fullPower status ({ starship } as r) =
    pilotCheckHelper (PilotResult.fullPower starship) CombatCrew.fullPower status r


audaciousGambit : Status -> { a | currentRound : Int, starship : Starship, useComputerNode : Bool } -> Maybe ( Status, Int )
audaciousGambit =
    pilotCheckHelper PilotResult.audaciousGambit CombatCrew.audaciousGambit


audaciousGambitFail : Status -> { a | currentRound : Int, starship : Starship, useComputerNode : Bool } -> Maybe ( Status, Int )
audaciousGambitFail =
    pilotCheckHelper PilotResult.audaciousGambitFail CombatCrew.audaciousGambit


getEffectiveAcAndTl : Starship -> Int -> Status -> ( Int, Int )
getEffectiveAcAndTl starship currentRound status =
    let
        pilot =
            status.assignments.pilot
                |> Maybe.andThen (\id -> Dict.get id status.crew)

        pilotAcBonus =
            CombatCrew.getAcModifier status.crew { assignments = status.assignments }

        pilotTlBonus =
            CombatCrew.getTlModifier status.crew { assignments = status.assignments }

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


getEffectiveDistanceBetweenTurns : Starship -> Int -> Status -> Maybe Int
getEffectiveDistanceBetweenTurns starship currentRound status =
    let
        starshipModifier =
            Starship.getDistanceBetweenTurnsModifier starship

        ( pilotResultRound, pilotResult ) =
            status.pilotResult

        mPilotEffect =
            if currentRound == pilotResultRound then
                PilotResult.getDistanceBetweenTurnsModifier pilotResult

            else
                Just 0
    in
    mPilotEffect
        |> Maybe.map (\pilotEffect -> max 0 (starshipModifier + pilotEffect))


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
                |> Maybe.map (Tuple.first >> f)
                |> Maybe.withDefault 0

        csMod =
            Dict.get crewId status.crew
                |> Maybe.map (Tuple.second >> (\x -> g x r))
                |> Maybe.withDefault 0
    in
    cMod + csMod


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


demandSource : Status -> { a | currentRound : Int, target : String, starship : Starship, useComputerNode : Bool } -> Maybe ( Status, Int )
demandSource status ({ currentRound, target } as reader) =
    let
        newStatus =
            -- TODO: This needs to return an intimidate bonus
            CombatCrew.demandSource status.crew { target = target, assignments = status.assignments, currentRound = currentRound }
    in
    Maybe.map3 (\x b1 ( y, b2 ) -> ( { status | crew = x, computerNodesUsed = y }, b1 + b2 ))
        newStatus
        (getEffectiveBonus False currentRound LifeSupport status)
        (getComputerNodeBonus status.computerNodesUsed reader)


demandTarget : Status -> { a | currentRound : Int, target : String } -> Maybe Status
demandTarget status ({ currentRound, target } as r) =
    CombatCrew.demandTarget status.crew r
        |> Maybe.map (\x -> { status | crew = x })



-- TODO: Encourage is interesting, because you can essentially get a +5 (DC
-- reduced by 5) for using the associated skill instead of diplomacy


encourageSource : Status -> { a | currentRound : Int, starship : Starship, useComputerNode : Bool } -> Maybe Status
encourageSource status ({ currentRound } as reader) =
    Maybe.map3 (\x _ ( y, _ ) -> { status | crew = x, computerNodesUsed = y })
        (CombatCrew.encourageSource status.crew { assignments = status.assignments, currentRound = currentRound })
        (getEffectiveBonus False currentRound LifeSupport status)
        (getComputerNodeBonus status.computerNodesUsed reader)


encourageTarget : Status -> { a | currentRound : Int, target : String } -> Maybe Status
encourageTarget status r =
    CombatCrew.encourageTarget status.crew r
        |> Maybe.map (\x -> { status | crew = x })


tauntSource : Status -> { a | currentRound : Int, starship : Starship, useComputerNode : Bool } -> Maybe ( Status, Int )
tauntSource status ({ currentRound } as r) =
    Maybe.map3 (\( newCrew, ( a, b ) ) b2 ( newComputerNodesUsed, b3 ) -> ( { status | crew = newCrew, computerNodesUsed = newComputerNodesUsed }, max a b + b2 + b3 ))
        (CombatCrew.tauntSource status.crew { currentRound = currentRound, assignments = status.assignments })
        (getEffectiveBonus True currentRound LifeSupport status)
        (getComputerNodeBonus status.computerNodesUsed r)


tauntTarget : Status -> { a | source : String, currentRound : Int, taunted : Taunted } -> Maybe ( Status, Int )
tauntTarget status { source, currentRound, taunted } =
    if Dict.member source status.tauntedBy then
        Nothing

    else
        Just ( { status | tauntedBy = Dict.insert source ( currentRound, taunted ) status.tauntedBy }, 0 )



-- TODO: Orders is interesting, because you need to use the skill that target
-- character will use


ordersSource : Status -> { a | currentRound : Int, starship : Starship, useComputerNode : Bool } -> Maybe Status
ordersSource status ({ currentRound } as r) =
    Maybe.map3 (\x _ ( y, _ ) -> { status | crew = x, computerNodesUsed = y })
        (CombatCrew.ordersSource status.crew { currentRound = currentRound, assignments = status.assignments })
        (getEffectiveBonus True currentRound LifeSupport status)
        (getComputerNodeBonus status.computerNodesUsed r)



-- TODO: This only affects a single crewmate (and the captain can't order
-- himself)


ordersTarget : Status -> { a | currentRound : Int, target : String } -> Maybe Status
ordersTarget status r =
    CombatCrew.ordersTarget status.crew r
        |> Maybe.map (\x -> { status | crew = x })



-- TODO: This is similar to orders, like _really_ similar, but the true
-- abstraction just doesn't quite seem obvious


movingSpeechSource : Status -> { a | currentRound : Int, starship : Starship, useComputerNode : Bool } -> Maybe ( Status, Int )
movingSpeechSource status ({ currentRound } as r) =
    let
        mNewCrewAndBonus =
            CombatCrew.movingSpeechSource status.crew { currentRound = currentRound, assignments = status.assignments }

        mNonCrewBonus =
            getEffectiveBonus True currentRound LifeSupport status
    in
    Maybe.map3 (\( a, b1 ) b2 ( c, b3 ) -> ( { status | crew = a, computerNodesUsed = c }, b1 + b2 + b3 ))
        mNewCrewAndBonus
        mNonCrewBonus
        (getComputerNodeBonus status.computerNodesUsed r)


movingSpeechTarget : Status -> { a | currentRound : Int, currentPhase : CombatPhase } -> ( Status, Int )
movingSpeechTarget status r =
    --TODO This effect should be moved from CrewStatus to Status as it affects the entire ship.
    ( status, 0 )



-- TODO: All these ignore computer nodes


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
