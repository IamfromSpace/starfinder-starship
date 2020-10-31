module Status exposing (Assignments, ExtraPoweredSystem(..), Status, areShieldsFull, balanceEvenly, balanceFromArc, basePatchDC, canBalanceFromTo, damageArc, damageSystem, divertPowerToEngines, divertPowerToShields, forceAddShields, forceMoveShields, getEffectiveAcAndTl, getEffectiveBonus, getEffectiveDistanceBetweenTurns, getEffectiveSpeed, hasExtraPower, holdItTogether, init, maxDivertPowerToShieldPoints, patch, quickFix)

import Arc exposing (AnArc, Arc)
import Crewmate exposing (Crewmate)
import CriticalStatus as CS exposing (CriticalStatus, PatchEffectiveness(..), Severity(..))
import DefenseLevel
import Dict exposing (Dict)
import PatchableSystems as PS exposing (PatchableSystem(..), PatchableSystems)
import PilotResult exposing (PilotResult, noPilotResult)
import Size
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


type alias Assignments a =
    { captain : Maybe a
    , pilot : Maybe a
    , engineers : List a
    , scienceOfficers : List a
    , gunners : List a
    }



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
    , pilotResult : ( Int, PilotResult )

    -- TODO: Ideally CrewId (or just an "assignable thing") isn't a String, but
    -- a parameterized type so that it can be any identifier, or a
    -- CrewmateStatus, or a (Crewmate, CrewmateStatus), or something else.
    -- However, this parameterization gets _everywhere_, so it's a bit of apain
    -- to do now.
    , crew : Dict String Crewmate
    , assignments : Assignments String
    , tauntedBy : Dict String ( Int, Taunted )
    }


init : Status
init =
    { damage = 0
    , shields = Arc.pure 0
    , systems = PS.pure Nothing
    , powerAction = ( -1, Divert Shields ) -- The -1 round acts as a No-op
    , pilotResult = ( -1, noPilotResult ) -- The -1 round acts as a No-op
    , crew = Dict.empty
    , assignments =
        { captain = Nothing
        , pilot = Nothing
        , scienceOfficers = []
        , engineers = []
        , gunners = []
        }
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


getEffectiveBonus : Int -> PatchableSystem -> Bool -> Status -> Maybe Int
getEffectiveBonus currentRound involvedSystem isPush status =
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

        conditionallyIncludePowerCorEffects =
            case involvedSystem of
                PowerCore ->
                    identity

                _ ->
                    (+) powerCoreEffectBonus
    in
    Maybe.map ((+) tauntedByBonus >> conditionallyIncludePowerCorEffects) standardBonus


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


maneuver : Starship -> Int -> Status -> Status
maneuver =
    applyPilotResult (always PilotResult.maneuver)


backOff : Starship -> Int -> Status -> Status
backOff =
    applyPilotResult PilotResult.backOff


backOffFail : Starship -> Int -> Status -> Status
backOffFail =
    applyPilotResult (always PilotResult.backOffFail)


backOffFailBy5OrMore : Starship -> Int -> Status -> Status
backOffFailBy5OrMore =
    applyPilotResult PilotResult.backOffFailBy5OrMore


barrelRoll : Starship -> Int -> Status -> Status
barrelRoll =
    -- TODO: Only valid for ships Large or smaller
    applyPilotResult PilotResult.barrelRoll


barrelRollFail : Starship -> Int -> Status -> Status
barrelRollFail =
    -- TODO: Only valid for ships Large or smaller
    applyPilotResult PilotResult.barrelRollFail


barrelRollFailBy5OrMore : Starship -> Int -> Status -> Status
barrelRollFailBy5OrMore =
    -- TODO: Only valid for ships Large or smaller
    applyPilotResult PilotResult.barrelRollFailBy5OrMore


evade : Starship -> Int -> Status -> Status
evade =
    applyPilotResult (always PilotResult.evade)


evadeFailBy5OrMore : Starship -> Int -> Status -> Status
evadeFailBy5OrMore =
    applyPilotResult (always PilotResult.evadeFailBy5OrMore)


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
