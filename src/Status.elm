module Status exposing (Assignments, CriticalStatus, ExtraPoweredSystem(..), PatchEffectiveness(..), PatchableSystem(..), Severity(..), Status, areShieldsFull, balanceEvenly, balanceFromArc, basePatchDC, canBalanceFromTo, damage, damageArc, damageSeverity, damageSystem, divertPowerToEngines, divertPowerToShields, forceAddShields, forceMoveShields, getEffectiveAcAndTl, getEffectiveBonus, getEffectiveDistanceBetweenTurns, getEffectiveSeverity, getEffectiveSpeed, hasExtraPower, holdItTogether, init, maxDivertPowerToShieldPoints, patchCount, patchCriticalStatus, patchProgress, patchStatus, pickPatchableSystem, quickFix, updateCriticalStatus)

import Arc exposing (AnArc, Arc)
import Crewmate exposing (Crewmate)
import DefenseLevel
import Dict exposing (Dict)
import Random exposing (Generator)
import Size
import Starship exposing (Starship)
import Switch exposing (Switch(..))
import Togglable exposing (extract, meta)


type Severity
    = Glitching
    | Malfunctioning
    | Wrecked



-- Patches form a sort of Paneo numeral chain, where the stack is the number of
-- patches required to move to the next.  So W (M (M (G R))) represents a still
-- Wrecked condition, but with only one remaining patch left to move to
-- Malfunctioning.  This is helpful because engineers can take actions that
-- apply multiple patches, but only to the same level of condition.


type Patches
    = W Patches
    | M Patches
    | G Patches
    | R


type PatchEffectiveness
    = Single
    | Double
    | Triple


type CausalSeverity
    = Emp Int
    | CriticalDamage Severity


type ExtraPoweredSystem
    = EngineSpeed
    | ScienceEquipment
    | Weapons
    | Shields


type PowerAction
    = Divert ExtraPoweredSystem
    | Overpower (List ExtraPoweredSystem) -- UUUUUUUUGH The Elm maintainers are so stupid sometimes.


type SpecialPilotResult
    = MoveForwardFullSpeed
    | Turn180AtTheEnd
    | MoveBackwardExactlyOneHex
    | MoveBackward
    | SwapPortAndStarboard
    | NoFreeAttackForSingleEnemy
    | NoFreeAttackForAnyEnemyAndFreeFinalRotation
    | Slide
    | TurnInPlace


type alias PilotResult =
    { acAndTlBonus : Int
    , speedDelta : Int
    , maneuverabilityDelta : Int
    , special : Maybe SpecialPilotResult
    }


noPilotResult : PilotResult
noPilotResult =
    { acAndTlBonus = 0
    , speedDelta = 0
    , maneuverabilityDelta = 0
    , special = Nothing
    }


type alias CriticalStatus =
    { severity : CausalSeverity
    , patches : Patches
    , heldTogether :
        -- Holding together reduces two levels of severity for one round
        Maybe Int
    , quickFixed :
        -- QuickFix ignores this affect for 1 hour (the duration of combat)
        Bool
    }


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
    , lifeSupport : Maybe CriticalStatus
    , sensors : Maybe CriticalStatus
    , weaponsArray : Arc (Maybe CriticalStatus)
    , engines : Maybe CriticalStatus
    , powerCore : Maybe CriticalStatus
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
    , lifeSupport = Nothing
    , sensors = Nothing
    , weaponsArray = Arc.pure Nothing
    , engines = Nothing
    , powerCore = Nothing
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


considerEmp : Int -> CausalSeverity -> Maybe Severity
considerEmp currentRound causalSeverity =
    case causalSeverity of
        Emp untilRound ->
            if currentRound <= untilRound then
                Just Glitching

            else
                Nothing

        CriticalDamage severity ->
            Just severity


patchCount_ : Patches -> Int
patchCount_ patches =
    case patches of
        W (W (W x)) ->
            0

        W (W (M x)) ->
            1

        W (M x) ->
            2

        M (M x) ->
            0

        M (G x) ->
            1

        G R ->
            0

        _ ->
            0


patchCount : CriticalStatus -> Int
patchCount =
    patchCount_ << .patches


patchProgress : Patches -> ( Int, Int )
patchProgress patches =
    case patches of
        W (W (W x)) ->
            ( 3, 3 )

        W (W (M x)) ->
            ( 2, 3 )

        W (M x) ->
            ( 1, 3 )

        M (M x) ->
            ( 2, 2 )

        M (G x) ->
            ( 1, 2 )

        G R ->
            ( 0, 1 )

        _ ->
            ( 0, 0 )


applyPatches : Patches -> Maybe Severity
applyPatches patches =
    case patches of
        W _ ->
            Just Wrecked

        M _ ->
            Just Malfunctioning

        G _ ->
            Just Glitching

        R ->
            Nothing


applyHoldTogether : Int -> Maybe Int -> Severity -> Maybe Severity
applyHoldTogether currentRound heldTogether severity =
    case ( heldTogether, severity ) of
        ( Just onRound, Wrecked ) ->
            if onRound == currentRound then
                Just Glitching

            else
                Just severity

        ( Just onRound, _ ) ->
            if onRound == currentRound then
                Nothing

            else
                Just severity

        ( Nothing, _ ) ->
            Just severity


getEffectiveSeverity : Int -> CriticalStatus -> Maybe Severity
getEffectiveSeverity currentRound cs =
    if cs.quickFixed then
        Nothing

    else
        considerEmp currentRound cs.severity
            |> Maybe.andThen (always (applyPatches cs.patches))
            |> Maybe.andThen (applyHoldTogether currentRound cs.heldTogether)


getEffectiveBonus : Int -> PatchableSystem -> Bool -> Status -> Maybe Int
getEffectiveBonus currentRound involvedSystem isPush status =
    let
        bonusBySystem system =
            getCriticalStatus system status
                |> Maybe.andThen (getEffectiveSeverity currentRound)

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


basePatchDCPatches : PatchEffectiveness -> Patches -> Maybe Int
basePatchDCPatches pe patches =
    case ( pe, patches ) of
        ( Single, G _ ) ->
            Just 10

        ( Single, M _ ) ->
            Just 15

        ( Double, M (M _) ) ->
            Just 20

        ( Single, W _ ) ->
            Just 20

        ( Double, W (W _) ) ->
            Just 25

        ( Triple, W (W (W _)) ) ->
            Just 30

        _ ->
            Nothing


basePatchDC : PatchEffectiveness -> CriticalStatus -> Maybe Int
basePatchDC pe cs =
    basePatchDCPatches pe cs.patches


patchSeverity : PatchEffectiveness -> Patches -> Maybe Patches
patchSeverity pe patches =
    case ( pe, patches ) of
        ( Single, G x ) ->
            Just x

        ( Single, M x ) ->
            Just x

        ( Double, M (M x) ) ->
            Just x

        ( Single, W x ) ->
            Just x

        ( Double, W (W x) ) ->
            Just x

        ( Triple, W (W (W x)) ) ->
            Just x

        _ ->
            Nothing


patchCriticalStatus : PatchEffectiveness -> CriticalStatus -> CriticalStatus
patchCriticalStatus pe criticalStatus =
    patchSeverity pe criticalStatus.patches
        |> Maybe.map (\p -> { criticalStatus | patches = p })
        |> Maybe.withDefault criticalStatus


damageSeverity : CausalSeverity -> Severity
damageSeverity severity =
    case severity of
        Emp _ ->
            Glitching

        CriticalDamage Glitching ->
            Malfunctioning

        CriticalDamage Malfunctioning ->
            Wrecked

        -- TODO: This causes crew damage, where a crew member randomly takes
        -- equal damage to what was dealt to the hull.
        CriticalDamage Wrecked ->
            Wrecked


damage : Maybe Int -> Maybe CriticalStatus -> CriticalStatus
damage mEmpUntilRound mCriticalStatus =
    case ( mEmpUntilRound, mCriticalStatus ) of
        ( Nothing, Just criticalStatus ) ->
            let
                severity =
                    damageSeverity criticalStatus.severity

                patches =
                    case severity of
                        Glitching ->
                            G R

                        Malfunctioning ->
                            M (M (G R))

                        Wrecked ->
                            W (W (W (M (M (G R)))))
            in
            { criticalStatus
                | severity = CriticalDamage severity
                , patches = patches
            }

        ( Just empUntilRound, Just criticalStatus ) ->
            case criticalStatus.severity of
                Emp currentEmpUntilRound ->
                    { criticalStatus
                        | severity = Emp <| max empUntilRound currentEmpUntilRound
                        , patches = G R
                    }

                _ ->
                    criticalStatus

        ( _, Nothing ) ->
            { severity =
                mEmpUntilRound
                    |> Maybe.map Emp
                    |> Maybe.withDefault (CriticalDamage Glitching)
            , patches = G R
            , heldTogether = Nothing
            , quickFixed = False
            }


type PatchableSystem
    = LifeSupport
    | Sensors
    | WeaponsArray AnArc
    | Engines
    | PowerCore



-- A generator that picks a system to damage based on the odds
-- of impacting that system.


pickPatchableSystem : Generator PatchableSystem
pickPatchableSystem =
    Random.weighted ( 10, LifeSupport )
        [ ( 20, Sensors )
        , ( 7.5, WeaponsArray Arc.Forward )
        , ( 7.5, WeaponsArray Arc.Aft )
        , ( 7.5, WeaponsArray Arc.Port )
        , ( 7.5, WeaponsArray Arc.Starboard )
        , ( 20, Engines )
        , ( 20, PowerCore )
        ]


getCriticalStatus : PatchableSystem -> Status -> Maybe CriticalStatus
getCriticalStatus system =
    case system of
        LifeSupport ->
            .lifeSupport

        Sensors ->
            .sensors

        WeaponsArray arc ->
            .weaponsArray >> Arc.getArc arc

        Engines ->
            .engines

        PowerCore ->
            .powerCore


updateCriticalStatus : (Maybe CriticalStatus -> Maybe CriticalStatus) -> PatchableSystem -> Status -> Status
updateCriticalStatus fn system status =
    case system of
        LifeSupport ->
            { status | lifeSupport = fn status.lifeSupport }

        Sensors ->
            { status | sensors = fn status.sensors }

        WeaponsArray arc ->
            { status | weaponsArray = Arc.updateArc fn arc status.weaponsArray }

        Engines ->
            { status | engines = fn status.engines }

        PowerCore ->
            { status | powerCore = fn status.powerCore }


patchStatus : PatchEffectiveness -> PatchableSystem -> Status -> Status
patchStatus pe =
    updateCriticalStatus (Maybe.map (patchCriticalStatus pe))


holdItTogether : Int -> PatchableSystem -> Status -> Status
holdItTogether currentRound =
    updateCriticalStatus (Maybe.map (\s -> { s | heldTogether = Just currentRound }))


quickFix : PatchableSystem -> Status -> Status
quickFix =
    updateCriticalStatus (Maybe.map (\s -> { s | quickFixed = True }))


damageSystem : Maybe Int -> PatchableSystem -> Status -> Status
damageSystem mEmpUntilRound =
    updateCriticalStatus (damage mEmpUntilRound >> Just)


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


reductionToHalfSpeedAndNoTurns : Starship -> ( Int, Int )
reductionToHalfSpeedAndNoTurns starship =
    let
        baseSpeed =
            -- TODO: this ignores if thrusters are off
            extract starship.thrusters

        finalSpeed =
            baseSpeed // 2
    in
    -- TODO: This also considers if power has been diverted to the engines (by
    -- adding 3 instead of 1) but ultimately this should just be represented as
    -- a Maybe or something.
    ( finalSpeed - baseSpeed, finalSpeed + 3 )


maneuver : Starship -> Int -> Status -> Status
maneuver =
    applyPilotResult (always { noPilotResult | maneuverabilityDelta = -1 })


backOff : Starship -> Int -> Status -> Status
backOff =
    applyPilotResult
        (\starship ->
            let
                ( speedDelta, maneuverabilityDelta ) =
                    reductionToHalfSpeedAndNoTurns starship
            in
            { noPilotResult
                | maneuverabilityDelta = maneuverabilityDelta
                , speedDelta = speedDelta
                , special = Just MoveBackward
            }
        )


backOffFail : Starship -> Int -> Status -> Status
backOffFail =
    applyPilotResult (always { noPilotResult | special = Just MoveBackwardExactlyOneHex })


backOffFailBy5OrMore : Starship -> Int -> Status -> Status
backOffFailBy5OrMore =
    applyPilotResult
        (\starship ->
            { noPilotResult
                | acAndTlBonus = -4

                -- TODO: this ignores if thrusters are off
                , speedDelta = -(extract starship.thrusters)
            }
        )


barrelRoll : Starship -> Int -> Status -> Status
barrelRoll =
    -- TODO: Only valid for ships Large or smaller
    applyPilotResult
        (\starship ->
            { noPilotResult
                | speedDelta =
                    Tuple.first <|
                        reductionToHalfSpeedAndNoTurns starship
                , special = Just SwapPortAndStarboard
            }
        )


barrelRollFail : Starship -> Int -> Status -> Status
barrelRollFail =
    -- TODO: Only valid for ships Large or smaller
    applyPilotResult
        (\starship ->
            { noPilotResult
                | speedDelta =
                    Tuple.first <|
                        reductionToHalfSpeedAndNoTurns starship
            }
        )


barrelRollFailBy5OrMore : Starship -> Int -> Status -> Status
barrelRollFailBy5OrMore =
    -- TODO: Only valid for ships Large or smaller
    applyPilotResult
        (\starship ->
            { noPilotResult
                | acAndTlBonus = -4
                , speedDelta =
                    Tuple.first <|
                        reductionToHalfSpeedAndNoTurns starship
            }
        )


evade : Starship -> Int -> Status -> Status
evade =
    applyPilotResult (always { noPilotResult | acAndTlBonus = 2 })


evadeFailBy5OrMore : Starship -> Int -> Status -> Status
evadeFailBy5OrMore =
    applyPilotResult (always { noPilotResult | acAndTlBonus = -2 })


flipAndBurn : Starship -> Int -> Status -> Status
flipAndBurn =
    applyPilotResult
        (\starship ->
            let
                ( speedDelta, maneuverabilityDelta ) =
                    reductionToHalfSpeedAndNoTurns starship
            in
            { noPilotResult
                | maneuverabilityDelta = maneuverabilityDelta
                , speedDelta = speedDelta
                , special = Just Turn180AtTheEnd
            }
        )


flipAndBurnFail : Starship -> Int -> Status -> Status
flipAndBurnFail =
    applyPilotResult
        (\starship ->
            let
                ( speedDelta, maneuverabilityDelta ) =
                    reductionToHalfSpeedAndNoTurns starship
            in
            { noPilotResult
                | maneuverabilityDelta = maneuverabilityDelta
                , speedDelta = speedDelta
                , special = Just MoveForwardFullSpeed
            }
        )


flyby : Starship -> Int -> Status -> Status
flyby =
    applyPilotResult
        (always { noPilotResult | special = Just NoFreeAttackForSingleEnemy })


flybyFail : Starship -> Int -> Status -> Status
flybyFail =
    -- Just updates the most recent pilot effect (but has none)
    applyPilotResult (always noPilotResult)


slide : Starship -> Int -> Status -> Status
slide =
    applyPilotResult (always { noPilotResult | special = Just Slide })


slideFail : Starship -> Int -> Status -> Status
slideFail =
    applyPilotResult
        (\starship ->
            let
                ( speedDelta, maneuverabilityDelta ) =
                    reductionToHalfSpeedAndNoTurns starship
            in
            { noPilotResult
                | maneuverabilityDelta = maneuverabilityDelta
                , speedDelta = speedDelta
            }
        )


turnInPlace : Starship -> Int -> Status -> Status
turnInPlace =
    applyPilotResult
        (\starship ->
            { noPilotResult
                | acAndTlBonus =
                    case starship.frame.maneuverability of
                        Starship.Clumsy ->
                            -4

                        Starship.Poor ->
                            -2

                        _ ->
                            0

                -- TODO: this ignores if thrusters are off
                , speedDelta = -(extract starship.thrusters)
            }
        )


fullPower : Starship -> Int -> Status -> Status
fullPower =
    -- TODO: Also costs a Resolve Point
    -- TODO: this ignores if thrusters are off
    applyPilotResult (\starship -> { noPilotResult | maneuverabilityDelta = 2, speedDelta = extract starship.thrusters // 3 })


audaciousGambit : Starship -> Int -> Status -> Status
audaciousGambit =
    -- TODO: Also costs a Resolve Point
    applyPilotResult
        (always
            { noPilotResult
                | maneuverabilityDelta = -2
                , special = Just NoFreeAttackForAnyEnemyAndFreeFinalRotation
            }
        )


audaciousGambitFail : Starship -> Int -> Status -> Status
audaciousGambitFail =
    -- TODO: Also costs a Resolve Point
    -- Just updates the most recent pilot effect (but has none)
    applyPilotResult (always noPilotResult)


getEffectiveAcAndTl : Starship -> Int -> Status -> ( Int, Int )
getEffectiveAcAndTl starship currentRound status =
    let
        pilotsPilotRanks =
            status.assignments.pilot
                |> Maybe.andThen (\id -> Dict.get id status.crew)
                |> Maybe.map .pilotingRanks
                |> Maybe.withDefault 0

        sizeBonus =
            Size.acAndTlBonus starship.frame.size

        ( pilotResultRound, pilotResult ) =
            status.pilotResult

        pilotResultBonus =
            if currentRound == pilotResultRound then
                pilotResult.acAndTlBonus

            else
                0

        baseValue =
            10 + pilotsPilotRanks + sizeBonus + pilotResultBonus

        armorBonus =
            starship.armor
                |> Maybe.map DefenseLevel.toBonus
                |> Maybe.withDefault 0

        armorTlPenalty =
            starship.armor
                |> Maybe.map DefenseLevel.toTlPenalty
                |> Maybe.withDefault 0

        countermeasuresBonus =
            starship.defensiveCountermeasures
                -- TODO: These have to be on
                |> Maybe.map (DefenseLevel.toBonus << extract)
                |> Maybe.withDefault 0
    in
    ( baseValue + armorBonus, baseValue + armorTlPenalty + countermeasuresBonus )


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
                pilotResult.speedDelta

            else
                0
    in
    -- TODO: These have to be on
    extract starship.thrusters + engineeringBonus + pilotBonus


getEffectiveDistanceBetweenTurns : Starship -> Int -> Status -> Int
getEffectiveDistanceBetweenTurns starship currentRound status =
    let
        baseValue =
            case starship.frame.maneuverability of
                Starship.Clumsy ->
                    4

                Starship.Poor ->
                    3

                Starship.Average ->
                    2

                Starship.Good ->
                    1

                Starship.Perfect ->
                    0

        armorManeuverabilityEffect =
            starship.armor
                |> Maybe.map DefenseLevel.toManeuverabilityPenalty
                |> Maybe.withDefault 0

        ( pilotResultRound, pilotResult ) =
            status.pilotResult

        pilotEffect =
            if currentRound == pilotResultRound then
                pilotResult.maneuverabilityDelta

            else
                0
    in
    max 0 (baseValue + armorManeuverabilityEffect + pilotEffect)
