module StatusEditor exposing (Model, Msg(..), colorTransition, criticalStatusToRgb, init, main, maybeSeverityToPercent, update, view)

import Arc exposing (AnArc(..))
import Assignments exposing (Assignments, allInEngineering)
import AssignmentsEditor
import Browser exposing (element)
import Color exposing (Color, blue, green, grey, red, yellow)
import Color.Convert exposing (colorToCssRgb)
import Color.Manipulate exposing (weightedMix)
import CombatPhase exposing (CombatPhase(..))
import CounterArc
import Crewmate exposing (Crewmate)
import CriticalStatus as CS exposing (CriticalStatus, Severity(..))
import Dict exposing (Dict)
import Fighter
import Html exposing (Html, button, div, input, text)
import Html.Attributes as A
import Html.Events as E
import PatchableSystems as PS exposing (PatchableSystem(..))
import PilotResult exposing (SpecialPilotResult(..))
import Platform.Cmd exposing (Cmd)
import Platform.Sub
import Random
import ShieldArc
import Shielded
import ShipAssets exposing (..)
import Starship exposing (Starship)
import Status exposing (Status)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Switch exposing (Switch(..))
import Togglable exposing (extract, meta)


type PartialState
    = Selected AnArc
    | Diverting (Arc.Arc Int)
    | Allotting (Arc.Arc Int)
    | Balancing ( Arc.AnArc, Arc.AnArc, Int )
    | None


maybeDiverting : PartialState -> Maybe (Arc.Arc Int)
maybeDiverting ps =
    case ps of
        Diverting x ->
            Just x

        _ ->
            Nothing


maybeSelected : PartialState -> Maybe AnArc
maybeSelected ps =
    case ps of
        Selected x ->
            Just x

        _ ->
            Nothing


maybeAllotting : PartialState -> Maybe (Arc.Arc Int)
maybeAllotting ps =
    case ps of
        Allotting x ->
            Just x

        _ ->
            Nothing


maybeBalancing : PartialState -> Maybe ( Arc.AnArc, Arc.AnArc, Int )
maybeBalancing ps =
    case ps of
        Balancing x ->
            Just x

        _ ->
            Nothing


type Phase
    = Assign
    | CP CombatPhase


nextPhase : Model -> Phase
nextPhase model =
    case model.phase of
        Assign ->
            CP Engineering

        CP Engineering ->
            CP Piloting

        CP Piloting ->
            CP Gunnery

        CP Gunnery ->
            Assign


phaseToString : Phase -> String
phaseToString phase =
    case phase of
        Assign ->
            "Assignments"

        CP Engineering ->
            "Engineering"

        CP Piloting ->
            "Piloting"

        CP Gunnery ->
            "Gunnery"


type alias Model =
    { status : Status
    , critsRemaining : Int
    , damageInput : Maybe Int
    , partialState : PartialState
    , roundNumber : Int
    , phase : Phase
    }


init : Dict String Crewmate -> Starship -> Model
init crew starship =
    { status = Status.init crew
    , critsRemaining = 0
    , damageInput = Nothing
    , partialState = Allotting (Arc.pure ((extract starship.shields).shieldPoints // 4))
    , roundNumber = 0
    , phase = Assign
    }


type Msg
    = Damage Int Bool
    | ApplyCrit PatchableSystem
    | SelectSheildArc AnArc
    | DeselectSheildArc
    | ChangeDamageInput (Maybe Int)
    | StartDivertToShields
    | EditDivertToShields AnArc (Int -> Int)
    | CancelDivertToShields
    | AcceptDivertToShields
    | DivertToEngines
    | EditAllotmentToShields AnArc (Int -> Int)
    | AcceptAllotmentToShields
    | Maneuver
    | BackOff
    | BackOffFail
    | BackOffFailBy5OrMore
    | BarrelRoll
    | BarrelRollFail
    | BarrelRollFailBy5OrMore
    | Evade
    | EvadeFailBy5OrMore
    | FlipAndBurn
    | FlipAndBurnFail
    | StartBalanceFromArc
    | EditBalanceFromArc AnArc Int
    | CancelBalanceFromArc
    | AcceptBalanceFromArc
    | BalanceEvenly
    | Patch CS.PatchEffectiveness PatchableSystem
    | HoldItTogether PatchableSystem
    | QuickFix PatchableSystem
    | MovingSpeech
    | SetAssignments (Assignments String)
    | NextPhase


update : Starship -> Msg -> Model -> ( Model, Cmd Msg )
update starship msg model =
    -- TODO: Restrict actions based on the current phase of the round
    case msg of
        Damage damage wasCrit ->
            case ( model.partialState, model.critsRemaining == 0 ) of
                -- This may be over defensive, but we lock the model
                -- while we are waiting for the random result to come back
                ( Selected arc, True ) ->
                    let
                        ( critCount, newStatus ) =
                            Status.damageArc
                                wasCrit
                                starship
                                arc
                                damage
                                model.status
                    in
                    ( { model
                        | status = newStatus
                        , critsRemaining = critCount
                        , partialState = None
                      }
                    , Cmd.batch
                        (List.repeat critCount
                            (Random.generate ApplyCrit
                                PS.pickPatchableSystem
                            )
                        )
                    )

                _ ->
                    ( model, Cmd.none )

        ApplyCrit system ->
            ( { model
                | status = Status.damageSystem Nothing system model.status
                , critsRemaining = model.critsRemaining - 1
              }
            , Cmd.none
            )

        SelectSheildArc arc ->
            case ( getDamagePercent starship model.status > 0, model.partialState ) of
                -- Shields are unselectable when the ship is dead
                ( False, _ ) ->
                    ( model, Cmd.none )

                -- Shields are unselectable when diverting power
                ( _, Diverting _ ) ->
                    ( model, Cmd.none )

                -- Shields are unselectable when allotting points
                ( _, Allotting _ ) ->
                    ( model, Cmd.none )

                -- Shields are unselectable when balancing from an arc
                ( _, Balancing _ ) ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | partialState = Selected arc }, Cmd.none )

        DeselectSheildArc ->
            case model.partialState of
                Selected _ ->
                    ( { model | partialState = None }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeDamageInput (Just x) ->
            if x > 0 then
                ( { model | damageInput = Just x }, Cmd.none )

            else
                ( model, Cmd.none )

        ChangeDamageInput Nothing ->
            ( { model | damageInput = Nothing }, Cmd.none )

        StartDivertToShields ->
            case model.partialState of
                None ->
                    ( { model | partialState = Diverting (Arc.pure 0) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EditDivertToShields arc f ->
            case model.partialState of
                Diverting added ->
                    ( { model
                        | partialState = Diverting (Arc.updateArc f arc added)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        CancelDivertToShields ->
            case model.partialState of
                Diverting _ ->
                    ( { model | partialState = None }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AcceptDivertToShields ->
            case model.partialState of
                Diverting added ->
                    ( Status.divertPowerToShields starship added model.roundNumber model.status
                        |> Maybe.map
                            (\newStatus ->
                                { model | partialState = None, status = newStatus }
                            )
                        |> Maybe.withDefault model
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        -- TODO: You're only allowed to route extra power to one system via
        -- divert or three via overpower
        DivertToEngines ->
            case model.partialState of
                None ->
                    ( { model
                        | status =
                            Status.divertPowerToEngines starship model.roundNumber model.status
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        EditAllotmentToShields arc f ->
            case model.partialState of
                Allotting allotment ->
                    ( { model
                        | partialState = Allotting (Arc.updateArc f arc allotment)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        AcceptAllotmentToShields ->
            case model.partialState of
                Allotting allotment ->
                    let
                        newStatus =
                            Status.forceAddShields allotment model.status
                    in
                    ( if Status.areShieldsFull starship newStatus then
                        { model | partialState = None, status = newStatus }

                      else
                        model
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Maneuver ->
            case ( model.phase, Status.maneuver model.status { currentRound = model.roundNumber } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        BackOff ->
            case ( model.phase, Status.backOff model.status { currentRound = model.roundNumber, starship = starship } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        BackOffFail ->
            case ( model.phase, Status.backOffFail model.status { currentRound = model.roundNumber } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        BackOffFailBy5OrMore ->
            case ( model.phase, Status.backOffFailBy5OrMore model.status { currentRound = model.roundNumber, starship = starship } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        BarrelRoll ->
            case ( model.phase, Status.barrelRoll model.status { currentRound = model.roundNumber, starship = starship } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        BarrelRollFail ->
            case ( model.phase, Status.barrelRollFail model.status { currentRound = model.roundNumber, starship = starship } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        BarrelRollFailBy5OrMore ->
            case ( model.phase, Status.barrelRollFailBy5OrMore model.status { currentRound = model.roundNumber, starship = starship } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Evade ->
            case ( model.phase, Status.evade model.status { currentRound = model.roundNumber } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EvadeFailBy5OrMore ->
            case ( model.phase, Status.evadeFailBy5OrMore model.status { currentRound = model.roundNumber, starship = starship } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FlipAndBurn ->
            case ( model.phase, Status.flipAndBurn model.status { currentRound = model.roundNumber, starship = starship } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FlipAndBurnFail ->
            case ( model.phase, Status.flipAndBurnFail model.status { currentRound = model.roundNumber, starship = starship } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartBalanceFromArc ->
            case model.partialState of
                Selected arc ->
                    ( { model | partialState = Balancing ( arc, arc, 0 ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EditBalanceFromArc to amount ->
            case model.partialState of
                Balancing ( from, _, _ ) ->
                    ( { model
                        | partialState = Balancing ( from, to, amount )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        CancelBalanceFromArc ->
            case model.partialState of
                Balancing _ ->
                    ( { model | partialState = None }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AcceptBalanceFromArc ->
            case model.partialState of
                Balancing change ->
                    ( Status.balanceFromArc starship change model.status
                        |> Maybe.map
                            (\newStatus ->
                                { model | partialState = None, status = newStatus }
                            )
                        |> Maybe.withDefault model
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        BalanceEvenly ->
            ( { model | status = Status.balanceEvenly starship model.status }, Cmd.none )

        Patch pe patchableSystem ->
            ( { model | status = Status.patch pe patchableSystem model.status }, Cmd.none )

        HoldItTogether patchableSystem ->
            ( { model | status = Status.holdItTogether model.roundNumber patchableSystem model.status }, Cmd.none )

        QuickFix patchableSystem ->
            ( { model | status = Status.quickFix patchableSystem model.status }, Cmd.none )

        MovingSpeech ->
            case model.phase of
                CP combatPhase ->
                    let
                        r =
                            { currentRound = model.roundNumber, currentPhase = combatPhase }

                        ( afterTargetStatus, _ ) =
                            Status.movingSpeechTarget model.status r

                        newModel =
                            Status.movingSpeechSource afterTargetStatus r
                                |> Maybe.map Tuple.first
                                |> Maybe.map (\s -> { model | status = s })
                                |> Maybe.withDefault model
                    in
                    ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetAssignments a ->
            let
                s =
                    model.status
            in
            ( { model | status = { s | assignments = a } }, Cmd.none )

        NextPhase ->
            ( { model
                | roundNumber =
                    model.roundNumber
                        + (case model.phase of
                            CP Gunnery ->
                                1

                            _ ->
                                0
                          )
                , phase = nextPhase model
              }
            , Cmd.none
            )


colorTransition : Float -> Color
colorTransition x =
    if x > 1 then
        weightedMix Color.blue green ((x - 1) / 1.8)

    else if x > 0.5 then
        weightedMix green yellow (2 * (x - 0.5))

    else if x > 0 then
        weightedMix yellow red (2 * x)

    else
        grey


maybeSeverityToPercent : Maybe Severity -> Float
maybeSeverityToPercent mSeverity =
    case mSeverity of
        Nothing ->
            1

        Just severity ->
            case severity of
                Glitching ->
                    0.67

                Malfunctioning ->
                    0.33

                Wrecked ->
                    0.001


criticalStatusToRgb : Int -> Maybe CriticalStatus -> Color
criticalStatusToRgb roundNumber =
    Maybe.andThen (CS.getEffectiveSeverity roundNumber)
        >> maybeSeverityToPercent
        >> colorTransition


getDamagePercent starship status =
    let
        hp =
            Starship.getMaxHitPoints starship
    in
    toFloat (hp - status.damage) / toFloat hp


selectedShieldedFighter : Color -> Color -> (Arc.AnArc -> a) -> Arc.AnArc -> Float -> Svg a
selectedShieldedFighter unselectedColor selectedColor onClicks selected size =
    Shielded.view
        (\size_ color ->
            Fighter.view
                { size = size_
                , color = color
                }
        )
        { size = size
        , arcColors =
            Arc.mapWithAnArc
                (\arc _ ->
                    if arc == selected then
                        selectedColor

                    else
                        unselectedColor
                )
                (Arc.pure ())
        , arcOnClick = onClicks
        , shielded = unselectedColor
        }


shieldedFighter : Starship -> Status -> (Arc.AnArc -> a) -> Arc.Arc (Maybe a) -> Arc.Arc (Maybe a) -> Float -> Svg a
shieldedFighter starship status arcOnClick onPlus onMinus size =
    let
        damagePercent =
            getDamagePercent starship status

        shieldDamagePercents =
            Arc.map
                (\points ->
                    if damagePercent > 0 && meta starship.shields == On then
                        toFloat points
                            / (toFloat (extract starship.shields).shieldPoints / 4)

                    else
                        0
                )
                status.shields
    in
    Svg.g
        []
        [ Shielded.view
            (\size_ color ->
                Fighter.view
                    { size = size_
                    , color = color
                    }
            )
            { size = size
            , arcColors = Arc.map colorTransition shieldDamagePercents
            , arcOnClick = arcOnClick
            , shielded = colorTransition damagePercent
            }
        , CounterArc.view
            { radius = size * 49 / 100
            , size = size / 12.5
            , offset = ( size / 2, size / 2 )
            , color = Color.black
            , backgroundColor = grey
            , counts = status.shields
            , onPlus = onPlus
            , onMinus = onMinus
            }
        ]


divertingShieldedFighter : Starship -> Status -> Arc.Arc Int -> Float -> Svg Msg
divertingShieldedFighter starship status added size =
    let
        statusWithNew =
            Status.forceAddShields added status

        onPlus =
            let
                enabled =
                    if Status.maxDivertPowerToShieldPoints starship status > Arc.sum added then
                        Just ()

                    else
                        Nothing
            in
            Arc.pureWithAnArc
                (\arc -> Maybe.map (always (EditDivertToShields arc ((+) 1))) enabled)

        onMinus =
            Arc.pureWithAnArc
                (\arc ->
                    -- Enabled as long as we don't push the arc below zero.
                    if Arc.getArc arc added > 0 then
                        Just (EditDivertToShields arc (\x -> x - 1))

                    else
                        Nothing
                )
    in
    shieldedFighter starship statusWithNew SelectSheildArc onPlus onMinus size


allottingShieldedFighter : Starship -> Status -> Arc.Arc Int -> Float -> Svg Msg
allottingShieldedFighter starship status allotment size =
    let
        statusWithAllotted =
            Status.forceAddShields allotment status

        onPlus =
            let
                enabled =
                    if Status.areShieldsFull starship statusWithAllotted then
                        Nothing

                    else
                        Just ()
            in
            Arc.pureWithAnArc
                (\arc -> Maybe.map (always (EditAllotmentToShields arc ((+) 1))) enabled)

        onMinus =
            let
                minPoints =
                    (extract starship.shields).shieldPoints // 10
            in
            Arc.pureWithAnArc
                (\arc ->
                    -- Enabled as long as we don't push the arc below zero.
                    if Arc.getArc arc allotment > minPoints then
                        Just (EditAllotmentToShields arc (\x -> x - 1))

                    else
                        Nothing
                )
    in
    shieldedFighter starship statusWithAllotted SelectSheildArc onPlus onMinus size


balancingShieldedFighter : Starship -> Status -> ( Arc.AnArc, Arc.AnArc, Int ) -> Float -> Svg Msg
balancingShieldedFighter starship status ( from, to, amount ) size =
    let
        tenPercent =
            Arc.sum status.shields // 10

        statusWithAltered =
            Status.forceMoveShields ( from, to, amount ) status

        validToArcs =
            Status.canBalanceFromTo from status.shields

        onPlus =
            Arc.pureWithAnArc
                (\arc ->
                    let
                        fromHasExtra =
                            Arc.getArc from statusWithAltered.shields > tenPercent

                        isCurrentSelection =
                            amount /= 0 && arc == to

                        isSelectable =
                            amount == 0 && List.member arc validToArcs
                    in
                    if arc /= from && fromHasExtra && (isCurrentSelection || isSelectable) then
                        Just (EditBalanceFromArc arc (amount + 1))

                    else
                        Nothing
                )

        onMinus =
            Arc.pureWithAnArc
                (\arc ->
                    if arc == to && amount > 0 then
                        Just (EditBalanceFromArc to (amount - 1))

                    else
                        Nothing
                )
    in
    shieldedFighter starship statusWithAltered SelectSheildArc onPlus onMinus size


view : Starship -> Model -> Html Msg
view starship model =
    let
        size =
            200

        sizeStr =
            String.fromFloat size

        damagePercent =
            getDamagePercent starship model.status

        isStillAllotting =
            maybeAllotting model.partialState
                |> Maybe.map (\s -> Status.forceAddShields s model.status)
                |> Maybe.map (Status.areShieldsFull starship)
                |> Maybe.withDefault False

        ( effectiveAc, effectiveTl ) =
            Status.getEffectiveAcAndTl starship model.roundNumber model.status

        patchableDisplay isEngineeringPhase name status patchableSystem =
            let
                effectiveSeverity =
                    Maybe.andThen (CS.getEffectiveSeverity model.roundNumber) status

                impacted =
                    effectiveSeverity == Nothing

                xOfYStr ( x, y ) =
                    "(" ++ String.fromInt x ++ "/" ++ String.fromInt y ++ ")"

                -- TODO: This helpful when Held Together (patches will be
                -- useful next round) but not so helpful when quickfixed
                -- (patches accomplish nothing).
                patchDisplay =
                    status
                        |> Maybe.map (.patches >> CS.patchProgress >> xOfYStr)
                        |> Maybe.withDefault ""
            in
            div
                [ A.style
                    "background-color"
                    (colorToCssRgb <|
                        if damagePercent > 0 then
                            criticalStatusToRgb model.roundNumber status

                        else
                            grey
                    )
                ]
                [ button
                    [ A.disabled (Maybe.andThen (CS.basePatchDC CS.Single) status == Nothing || not isEngineeringPhase)
                    , A.title "PATCH: apply a single patch towards the repair of one level of severity."
                    , E.onClick (Patch CS.Single patchableSystem)
                    ]
                    [ text "P" ]
                , button
                    [ A.disabled (Maybe.andThen (CS.basePatchDC CS.Double) status == Nothing || not isEngineeringPhase)
                    , A.title "PATCH: apply two patches towards the repair of one level of severity."
                    , E.onClick (Patch CS.Double patchableSystem)
                    ]
                    [ text "Px2" ]
                , button
                    [ A.disabled (Maybe.andThen (CS.basePatchDC CS.Triple) status == Nothing || not isEngineeringPhase)
                    , A.title "PATCH: apply three patches towards the repair of one level of severity."
                    , E.onClick (Patch CS.Triple patchableSystem)
                    ]
                    [ text "Px3" ]
                , button
                    [ A.disabled (impacted || not isEngineeringPhase)
                    , A.title "HOLD IT TOGETHER: temporarily repair two levels of severtity for a single round."
                    , E.onClick (HoldItTogether patchableSystem)
                    ]
                    [ text "H" ]
                , button
                    [ A.disabled (impacted || not isEngineeringPhase)
                    , A.title "QUICK FIX (1RP): Completely ignore all critical damage for this system for 1 hour."
                    , E.onClick (QuickFix patchableSystem)
                    ]
                    [ text "Q" ]
                , text (name ++ " " ++ patchDisplay)
                ]
    in
    div []
        [ Svg.svg
            [ SA.height sizeStr
            , SA.width sizeStr
            , SA.viewBox <| "0 0 " ++ sizeStr ++ " " ++ sizeStr
            ]
            [ case model.partialState of
                None ->
                    shieldedFighter starship model.status SelectSheildArc (Arc.pure Nothing) (Arc.pure Nothing) size

                Selected selected ->
                    selectedShieldedFighter Color.grey
                        Color.black
                        (\arc ->
                            if arc == selected then
                                DeselectSheildArc

                            else
                                SelectSheildArc arc
                        )
                        selected
                        size

                Diverting added ->
                    divertingShieldedFighter starship model.status added size

                Allotting allotment ->
                    allottingShieldedFighter starship model.status allotment size

                Balancing balance ->
                    balancingShieldedFighter starship model.status balance size
            ]
        , div [] [ text ("AC: " ++ String.fromInt effectiveAc) ]
        , div [] [ text ("TL: " ++ String.fromInt effectiveTl) ]
        , div [] [ text ("Speed (hexes): " ++ String.fromInt (Status.getEffectiveSpeed starship model.roundNumber model.status)) ]
        , div [] [ text ("Turn: " ++ String.fromInt (Status.getEffectiveDistanceBetweenTurns starship model.roundNumber model.status)) ]
        , div []
            [ text
                ("Special: "
                    ++ (case Status.getEffectiveSpecialPilotResult model.status { currentRound = model.roundNumber } of
                            Just MoveForwardFullSpeed ->
                                "Move forward at full speed"

                            Just Turn180AtTheEnd ->
                                "Turn 180deg at the end of movement"

                            Just MoveBackwardExactlyOneHex ->
                                "Move backward exactly one hex"

                            Just MoveBackward ->
                                "Move backwards"

                            Just SwapPortAndStarboard ->
                                "Swap port and starboard arcs"

                            Just NoFreeAttackForSingleEnemy ->
                                "No free attack given"

                            Just NoFreeAttackForAnyEnemyAndFreeFinalRotation ->
                                "No free attack given, and ship can be freely rotated at the end of its movement"

                            Just Slide ->
                                "Slide"

                            Just TurnInPlace ->
                                "Turn in place"

                            Nothing ->
                                "None"
                       )
                )
            ]
        , input
            [ A.value (Maybe.map String.fromInt model.damageInput |> Maybe.withDefault "")
            , A.disabled
                (case ( model.partialState, model.phase ) of
                    ( Selected _, CP Gunnery ) ->
                        False

                    _ ->
                        True
                )
            , E.onInput (String.toInt >> ChangeDamageInput)
            , A.type_ "number"
            ]
            []

        -- TODO: Allow weapon effects (most notably EMP)
        , button
            (case ( model.partialState, model.damageInput, model.phase ) of
                ( Selected arc, Just damageInput, CP Gunnery ) ->
                    [ E.onClick (Damage damageInput True) ]

                _ ->
                    [ A.disabled True ]
            )
            [ text "Damage w/Crit" ]
        , button
            (case ( model.partialState, model.damageInput, model.phase ) of
                ( Selected arc, Just damageInput, CP Gunnery ) ->
                    [ E.onClick (Damage damageInput False) ]

                _ ->
                    [ A.disabled True ]
            )
            [ text "Damage" ]
        , case ( model.phase, Status.maneuver model.status { currentRound = model.roundNumber } ) of
            ( CP Piloting, Just ( _, bonus ) ) ->
                button
                    [ E.onClick Maneuver ]
                    [ text ("Maneuver (" ++ String.fromInt bonus ++ ")") ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Maneuver" ]
        , case ( model.phase, Status.backOff model.status { currentRound = model.roundNumber, starship = starship } ) of
            ( CP Piloting, Just ( _, bonus ) ) ->
                button
                    [ E.onClick BackOff ]
                    [ text ("Back Off (" ++ String.fromInt bonus ++ ")") ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Back Off" ]
        , case ( model.phase, Status.backOffFail model.status { currentRound = model.roundNumber } ) of
            ( CP Piloting, Just _ ) ->
                button
                    [ E.onClick BackOffFail ]
                    [ text "Back Off (Fail)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Back Off (Fail)" ]
        , case ( model.phase, Status.backOffFailBy5OrMore model.status { currentRound = model.roundNumber, starship = starship } ) of
            ( CP Piloting, Just _ ) ->
                button
                    [ E.onClick BackOffFailBy5OrMore ]
                    [ text "Back Off (Fail by 5 or More)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Back Off (Fail by 5 or More)" ]
        , case ( model.phase, Status.barrelRoll model.status { currentRound = model.roundNumber, starship = starship } ) of
            ( CP Piloting, Just ( _, bonus ) ) ->
                button
                    [ E.onClick BarrelRoll ]
                    [ text ("Barrel Roll (" ++ String.fromInt bonus ++ ")") ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Barrel Roll" ]
        , case ( model.phase, Status.barrelRollFail model.status { currentRound = model.roundNumber, starship = starship } ) of
            ( CP Piloting, Just _ ) ->
                button
                    [ E.onClick BarrelRollFail ]
                    [ text "Barrel Roll (Fail)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Barrel Roll (Fail)" ]
        , case ( model.phase, Status.barrelRollFailBy5OrMore model.status { currentRound = model.roundNumber, starship = starship } ) of
            ( CP Piloting, Just _ ) ->
                button
                    [ E.onClick BarrelRollFailBy5OrMore ]
                    [ text "Barrel Roll (Fail by 5 or More)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Barrel Roll (Fail by 5 or More)" ]
        , case ( model.phase, Status.evade model.status { currentRound = model.roundNumber, starship = starship } ) of
            ( CP Piloting, Just ( _, bonus ) ) ->
                button
                    [ E.onClick Evade ]
                    [ text ("Evade (" ++ String.fromInt bonus ++ ")") ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Evade" ]
        , case ( model.phase, Status.evadeFailBy5OrMore model.status { currentRound = model.roundNumber, starship = starship } ) of
            ( CP Piloting, Just _ ) ->
                button
                    [ E.onClick EvadeFailBy5OrMore ]
                    [ text "Evade (Fail by 5 or More)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Evade (Fail by 5 or More)" ]
        , case ( model.phase, Status.flipAndBurn model.status { currentRound = model.roundNumber, starship = starship } ) of
            ( CP Piloting, Just ( _, bonus ) ) ->
                button
                    [ E.onClick FlipAndBurn ]
                    [ text ("Flip and Burn (" ++ String.fromInt bonus ++ ")") ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Flip and Burn" ]
        , case ( model.phase, Status.flipAndBurnFail model.status { currentRound = model.roundNumber, starship = starship } ) of
            ( CP Piloting, Just _ ) ->
                button
                    [ E.onClick FlipAndBurn ]
                    [ text "Flip and Burn (Fail)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Flip and Burn (Fail)" ]
        , button
            (case ( model.partialState, model.phase ) of
                ( Selected arc, CP Piloting ) ->
                    case Status.canBalanceFromTo arc model.status.shields of
                        [] ->
                            [ A.disabled True ]

                        _ ->
                            [ E.onClick StartBalanceFromArc ]

                _ ->
                    [ A.disabled True ]
            )
            [ text "Balance to other Shields" ]
        , button
            (case model.partialState of
                Balancing _ ->
                    [ E.onClick CancelBalanceFromArc ]

                _ ->
                    [ A.disabled True ]
            )
            [ text "Cancel Balance" ]
        , button
            (case Maybe.andThen (\x -> Status.balanceFromArc starship x model.status) (maybeBalancing model.partialState) of
                Nothing ->
                    [ A.disabled True ]

                Just arc ->
                    [ E.onClick AcceptBalanceFromArc ]
            )
            [ text "Accept Balance to other Shields" ]
        , button
            (case ( model.partialState, model.phase ) of
                ( None, CP Piloting ) ->
                    [ E.onClick BalanceEvenly ]

                _ ->
                    [ A.disabled True ]
            )
            [ text "Balance Shields Evenly" ]
        , button
            (case ( model.partialState, Status.maxDivertPowerToShieldPoints starship model.status <= 0, model.phase ) of
                ( None, False, CP Engineering ) ->
                    [ E.onClick StartDivertToShields ]

                _ ->
                    [ A.disabled True ]
            )
            [ text "Divert Power to Shields" ]
        , button
            (case model.partialState of
                Diverting _ ->
                    [ E.onClick CancelDivertToShields ]

                _ ->
                    [ A.disabled True ]
            )
            [ text "Cancel Divert Power to Shields" ]
        , button
            (case Maybe.andThen (\x -> Status.divertPowerToShields starship x model.roundNumber model.status) (maybeDiverting model.partialState) of
                Nothing ->
                    [ A.disabled True ]

                Just arc ->
                    [ E.onClick AcceptDivertToShields ]
            )
            [ text "Accept Divert Power to Shields" ]
        , button
            (case ( model.partialState, model.phase ) of
                ( None, CP Engineering ) ->
                    [ E.onClick DivertToEngines ]

                _ ->
                    [ A.disabled True ]
            )
            [ text "Divert Power to Engines" ]
        , button
            [ E.onClick AcceptAllotmentToShields, A.disabled (not isStillAllotting) ]
            [ text "Accept Allotted Shields" ]
        , let
            mCombatPhase =
                case model.phase of
                    CP combatPhase ->
                        Just combatPhase

                    _ ->
                        Nothing
          in
          mCombatPhase
            |> Maybe.andThen
                (\combatPhase ->
                    let
                        r =
                            { currentRound = model.roundNumber, currentPhase = combatPhase }

                        ( s1, b1 ) =
                            Status.movingSpeechTarget model.status r
                    in
                    Status.movingSpeechSource s1 r
                        |> Maybe.map
                            (\( nextStatus, bonus ) ->
                                button
                                    [ E.onClick MovingSpeech ]
                                    [ text ("Moving Speech (" ++ String.fromInt (bonus + b1) ++ ")") ]
                            )
                )
            |> Maybe.withDefault
                (button
                    [ A.disabled True ]
                    [ text "Moving Speech" ]
                )
        , patchableDisplay (model.phase == CP Engineering) "Life Support" model.status.systems.lifeSupport LifeSupport
        , patchableDisplay (model.phase == CP Engineering) "Sensors" model.status.systems.sensors Sensors
        , patchableDisplay (model.phase == CP Engineering) "Weapons Array - Forward" model.status.systems.weaponsArray.forward (WeaponsArray Arc.Forward)
        , patchableDisplay (model.phase == CP Engineering) "Weapons Array - Aft" model.status.systems.weaponsArray.aft (WeaponsArray Arc.Aft)
        , patchableDisplay (model.phase == CP Engineering) "Weapons Array - Port" model.status.systems.weaponsArray.portSide (WeaponsArray Arc.Port)
        , patchableDisplay (model.phase == CP Engineering) "Weapons Array - Starboard" model.status.systems.weaponsArray.starboard (WeaponsArray Arc.Starboard)
        , patchableDisplay (model.phase == CP Engineering) "Engines" model.status.systems.engines Engines
        , patchableDisplay (model.phase == CP Engineering) "Power Core" model.status.systems.powerCore PowerCore
        , AssignmentsEditor.view (model.phase /= Assign) model.status.assignments
            |> Html.map SetAssignments
        , button
            [ E.onClick NextPhase, A.disabled isStillAllotting ]
            [ text ("PROCEED TO " ++ String.toUpper (phaseToString (nextPhase model)) ++ " PHASE") ]
        ]


main : Program () Model Msg
main =
    element
        { init = \_ -> ( init Dict.empty norikamaDropship, Cmd.none )
        , update = update norikamaDropship
        , view = view norikamaDropship
        , subscriptions = \_ -> Sub.none
        }
