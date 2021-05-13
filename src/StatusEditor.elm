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
import WeaponDescription


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
    , useComputerNode : Bool
    , damageInput : Maybe Int
    , partialState : PartialState
    , roundNumber : Int
    , phase : Phase
    }


init : Dict String Crewmate -> Starship -> Model
init crew starship =
    { status = Status.init crew
    , critsRemaining = 0
    , useComputerNode = False
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
    | ToggleUseComputerNode
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
    | Flyby
    | FlybyFail
    | Slide
    | SlideFail
    | TurnInPlace
    | FullPower
    | AudaciousGambit
    | AudaciousGambitFail
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

        ToggleUseComputerNode ->
            ( { model | useComputerNode = not model.useComputerNode }, Cmd.none )

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
            case ( model.phase, Status.maneuver model.status { currentRound = model.roundNumber, useComputerNode = model.useComputerNode, starship = starship } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        BackOff ->
            case ( model.phase, Status.backOff model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        BackOffFail ->
            case ( model.phase, Status.backOffFail model.status { currentRound = model.roundNumber, useComputerNode = model.useComputerNode, starship = starship } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        BackOffFailBy5OrMore ->
            case ( model.phase, Status.backOffFailBy5OrMore model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        BarrelRoll ->
            case ( model.phase, Status.barrelRoll model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        BarrelRollFail ->
            case ( model.phase, Status.barrelRollFail model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        BarrelRollFailBy5OrMore ->
            case ( model.phase, Status.barrelRollFailBy5OrMore model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Evade ->
            case ( model.phase, Status.evade model.status { currentRound = model.roundNumber, useComputerNode = model.useComputerNode, starship = starship } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EvadeFailBy5OrMore ->
            case ( model.phase, Status.evadeFailBy5OrMore model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FlipAndBurn ->
            case ( model.phase, Status.flipAndBurn model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FlipAndBurnFail ->
            case ( model.phase, Status.flipAndBurnFail model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Flyby ->
            case ( model.phase, Status.flyby model.status { currentRound = model.roundNumber, useComputerNode = model.useComputerNode, starship = starship } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FlybyFail ->
            case ( model.phase, Status.flybyFail model.status { currentRound = model.roundNumber, useComputerNode = model.useComputerNode, starship = starship } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Slide ->
            case ( model.phase, Status.slide model.status { currentRound = model.roundNumber, useComputerNode = model.useComputerNode, starship = starship } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SlideFail ->
            case ( model.phase, Status.slideFail model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TurnInPlace ->
            case ( model.phase, Status.turnInPlace model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FullPower ->
            case ( model.phase, Status.fullPower model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AudaciousGambit ->
            case ( model.phase, Status.audaciousGambit model.status { currentRound = model.roundNumber, useComputerNode = model.useComputerNode, starship = starship } ) of
                ( CP Piloting, Just ( newStatus, _ ) ) ->
                    ( { model | status = newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AudaciousGambitFail ->
            case ( model.phase, Status.audaciousGambitFail model.status { currentRound = model.roundNumber, useComputerNode = model.useComputerNode, starship = starship } ) of
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
                            { currentRound = model.roundNumber, currentPhase = combatPhase, starship = starship, useComputerNode = model.useComputerNode }

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
        hp =
            Starship.getMaxHitPoints starship

        fontSize =
            size / 12.5

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
            , size = fontSize
            , offset = ( size / 2, size / 2 )
            , color = Color.black
            , backgroundColor = grey
            , counts = status.shields
            , onPlus = onPlus
            , onMinus = onMinus
            }
        , Svg.text_
            [ SA.fontFamily "mono"
            , SA.textAnchor "middle"
            , SA.alignmentBaseline "middle"
            , SA.fontSize <| String.fromFloat fontSize
            , SA.fill "black"
            , SA.transform <| "translate(" ++ String.fromFloat (size / 2) ++ ", " ++ String.fromFloat (size / 2) ++ ")"
            ]
            [ Svg.text (String.fromInt (max 0 (hp - status.damage)) ++ "/" ++ String.fromInt hp) ]
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

        shipDCMod =
            (round (Starship.getTierFromBuildPoints (Starship.getStarshipBuildPoints starship)) * 3) // 2

        allotmentIsValid =
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
                    [ text ("P (DC " ++ Maybe.withDefault "--" (Maybe.map ((+) shipDCMod >> String.fromInt) (Maybe.andThen (CS.basePatchDC CS.Single) status)) ++ ")") ]
                , button
                    [ A.disabled (Maybe.andThen (CS.basePatchDC CS.Double) status == Nothing || not isEngineeringPhase)
                    , A.title "PATCH: apply two patches towards the repair of one level of severity."
                    , E.onClick (Patch CS.Double patchableSystem)
                    ]
                    [ text ("Px2 (DC " ++ Maybe.withDefault "--" (Maybe.map ((+) shipDCMod >> String.fromInt) (Maybe.andThen (CS.basePatchDC CS.Double) status)) ++ ")") ]
                , button
                    [ A.disabled (Maybe.andThen (CS.basePatchDC CS.Triple) status == Nothing || not isEngineeringPhase)
                    , A.title "PATCH: apply three patches towards the repair of one level of severity."
                    , E.onClick (Patch CS.Triple patchableSystem)
                    ]
                    [ text ("Px3 (DC " ++ Maybe.withDefault "--" (Maybe.map ((+) shipDCMod >> String.fromInt) (Maybe.andThen (CS.basePatchDC CS.Triple) status)) ++ ")") ]
                , button
                    [ A.disabled (impacted || not isEngineeringPhase)
                    , A.title "HOLD IT TOGETHER: temporarily repair two levels of severtity for a single round."
                    , E.onClick (HoldItTogether patchableSystem)
                    ]
                    [ text ("H (DC " ++ String.fromInt (15 + shipDCMod) ++ ")") ]
                , button
                    [ A.disabled (impacted || not isEngineeringPhase)
                    , A.title "QUICK FIX (1RP): Completely ignore all critical damage for this system for 1 hour."
                    , E.onClick (QuickFix patchableSystem)
                    ]
                    [ text ("Q (DC " ++ String.fromInt (20 + shipDCMod) ++ ")") ]
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
        , div []
            [ Status.getEffectiveDistanceBetweenTurns starship model.roundNumber model.status
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "None allowed"
                |> (++) "Turn: "
                |> text
            ]
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

                            Just PilotResult.Slide ->
                                "Slide"

                            Just PilotResult.TurnInPlace ->
                                "Turn in place"

                            Nothing ->
                                "None"
                       )
                )
            ]

        -- TODO: It would be nice to disable this if there are no available
        -- nodes.
        , div [] [ text "Use Computer?", input [ A.type_ "checkbox", A.checked model.useComputerNode, E.onClick ToggleUseComputerNode ] [] ]
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
        , case ( model.phase, Status.maneuver model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
            ( CP Piloting, Just ( _, bonus ) ) ->
                button
                    [ E.onClick Maneuver
                    , A.title "You move your starship up to its speed. You can also attempt a Piloting check (DC = 15 + 1-1/2 × your starship’s tier) to reduce your starship’s distance between turns by 1 (to a minimum of 0)."
                    ]
                    [ text ("Maneuver (" ++ String.fromInt bonus ++ " vs DC " ++ String.fromInt (15 + shipDCMod) ++ ")") ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Maneuver" ]
        , case ( model.phase, Status.backOff model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
            ( CP Piloting, Just ( _, bonus ) ) ->
                button
                    [ E.onClick BackOff
                    , A.title "The starship moves up to half its speed in the direction of the aft edge without changing facing. It can’t take any turns during this movement. To perform this stunt, you must succeed at a Piloting check (DC = 10 + 1½ × your starship’s tier). On a failed check, your starship moves backward only 1 hex. If you fail this check by 5 or more, your starship does not move at all and takes a –4 penalty to its AC and TL until the start of the next round."
                    ]
                    [ text ("Back Off (" ++ String.fromInt bonus ++ " vs DC " ++ String.fromInt (10 + shipDCMod) ++ ")") ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Back Off" ]
        , case ( model.phase, Status.backOffFail model.status { currentRound = model.roundNumber, useComputerNode = model.useComputerNode, starship = starship } ) of
            ( CP Piloting, Just _ ) ->
                button
                    [ E.onClick BackOffFail ]
                    [ text "Back Off (Fail)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Back Off (Fail)" ]
        , case ( model.phase, Status.backOffFailBy5OrMore model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
            ( CP Piloting, Just _ ) ->
                button
                    [ E.onClick BackOffFailBy5OrMore ]
                    [ text "Back Off (Fail by 5 or More)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Back Off (Fail by 5 or More)" ]
        , case ( model.phase, Status.barrelRoll model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
            ( CP Piloting, Just ( _, bonus ) ) ->
                button
                    [ E.onClick BarrelRoll
                    , A.title "The starship moves up to half its speed and flips along its central axis. For the next gunnery phase, the starship’s port shields and weapons function as if they were in the starboard firing arc and vice versa. The starship reverts to normal at the beginning of the next round. To perform this stunt, your starship must be Large or smaller and you must succeed at a Piloting check (DC = 10 + 1½ × your starship’s tier). On a failed check, the starship moves half its speed but doesn’t roll. If you fail by 5 or more, your starship moves half its speed, doesn’t roll, and takes a –4 penalty to its AC and TL until the start of the next round."
                    ]
                    [ text ("Barrel Roll (" ++ String.fromInt bonus ++ " vs DC " ++ String.fromInt (10 + shipDCMod) ++ ")") ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Barrel Roll" ]
        , case ( model.phase, Status.barrelRollFail model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
            ( CP Piloting, Just _ ) ->
                button
                    [ E.onClick BarrelRollFail ]
                    [ text "Barrel Roll (Fail)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Barrel Roll (Fail)" ]
        , case ( model.phase, Status.barrelRollFailBy5OrMore model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
            ( CP Piloting, Just _ ) ->
                button
                    [ E.onClick BarrelRollFailBy5OrMore ]
                    [ text "Barrel Roll (Fail by 5 or More)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Barrel Roll (Fail by 5 or More)" ]
        , case ( model.phase, Status.evade model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
            ( CP Piloting, Just ( _, bonus ) ) ->
                button
                    [ E.onClick Evade
                    , A.title "The ship moves up to its speed and can turn as normal, but it gains a +2 circumstance bonus to its AC and TL until the start of the next round. To perform this stunt, you must succeed at a Piloting check (DC = 10 + 1½ × your starship’s tier). If you fail, the starship moves as normal. If you fail the check by 5 or more, the starship moves as normal, but it also takes a –2 penalty to its AC and TL until the start of the next round."
                    ]
                    [ text ("Evade (" ++ String.fromInt bonus ++ " vs DC " ++ String.fromInt (10 + shipDCMod) ++ ")") ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Evade" ]
        , case ( model.phase, Status.evadeFailBy5OrMore model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
            ( CP Piloting, Just _ ) ->
                button
                    [ E.onClick EvadeFailBy5OrMore ]
                    [ text "Evade (Fail by 5 or More)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Evade (Fail by 5 or More)" ]
        , case ( model.phase, Status.flipAndBurn model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
            ( CP Piloting, Just ( _, bonus ) ) ->
                button
                    [ E.onClick FlipAndBurn
                    , A.title "The ship moves forward up to half its speed (without turning) and rotates 180 degrees to face the aft edge at the end of the movement. To perform this stunt, you must succeed at a Piloting check (DC = 15 + 1½ × your ship’s tier). If you fail this check, your starship moves forward half its speed but doesn’t rotate."
                    ]
                    [ text ("Flip and Burn (" ++ String.fromInt bonus ++ " vs DC " ++ String.fromInt (15 + shipDCMod) ++ ")") ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Flip and Burn" ]
        , case ( model.phase, Status.flipAndBurnFail model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
            ( CP Piloting, Just _ ) ->
                button
                    [ E.onClick FlipAndBurnFail ]
                    [ text "Flip and Burn (Fail)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Flip and Burn (Fail)" ]
        , case ( model.phase, Status.flyby model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
            ( CP Piloting, Just ( _, bonus ) ) ->
                button
                    [ E.onClick Flyby
                    , A.title "The ship moves as normal, but it can move through 1 hex occupied by an enemy starship without provoking a free attack (as described in Moving through Other Starships). During the following gunnery phase, you can select one arc of your starship’s weapons to fire at the enemy vessel as if the vessel were in close range (treat the range as 1 hex), against any quadrant of the enemy starship. To perform this stunt, you must succeed at a Piloting check (DC = 15 + 1½ × the tier of the enemy starship). If you fail this check, your starship still moves as described above, but you follow the normal rules for attacking (based on your starship’s final position and distance), and the movement provokes a free attack from that starship as normal."
                    ]
                    [ text ("Flyby (" ++ String.fromInt bonus ++ " vs DC " ++ String.fromInt (15 + shipDCMod) ++ ")") ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Flyby" ]
        , case ( model.phase, Status.flybyFail model.status { currentRound = model.roundNumber, useComputerNode = model.useComputerNode, starship = starship } ) of
            ( CP Piloting, Just _ ) ->
                button
                    [ E.onClick FlybyFail ]
                    [ text "Flyby (Fail)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Flyby (Fail)" ]
        , case ( model.phase, Status.slide model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
            ( CP Piloting, Just ( _, bonus ) ) ->
                button
                    [ E.onClick Slide
                    , A.title "The starship moves up to its speed in the direction of either the forward-port or forward-starboard edge without changing its facing. To perform this stunt, you must succeed at a Piloting check (DC = 10 + 1½ × your ship’s tier). If you fail this check, the ship moves forward up to half its speed and can’t make any turns."
                    ]
                    [ text ("Slide (" ++ String.fromInt bonus ++ " vs DC " ++ String.fromInt (10 + shipDCMod) ++ ")") ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Slide" ]
        , case ( model.phase, Status.slideFail model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
            ( CP Piloting, Just _ ) ->
                button
                    [ E.onClick SlideFail ]
                    [ text "Slide (Fail)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Slide (Fail)" ]
        , case ( model.phase, Status.turnInPlace model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
            ( CP Piloting, Just ( _, bonus ) ) ->
                button
                    [ E.onClick TurnInPlace
                    , A.title "The ship does not move but instead can turn to face any direction. If the ship has a maneuverability of clumsy, it takes a –4 penalty to its AC and TL until the start of the next round. If it has a maneuverability of poor, it instead takes a –2 penalty to its AC and TL until the start of the next round. Ships with a maneuverability of average or better do not take a penalty. This stunt doesn’t require a skill check."
                    ]
                    [ text "Turn in Place (no check)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Turn in Place" ]
        , case ( model.phase, Status.fullPower model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
            ( CP Piloting, Just ( _, bonus ) ) ->
                button
                    [ E.onClick FullPower
                    , A.title "If you have at least 6 ranks in Piloting, you can spend 1 Resolve Point to move your starship up to 1-1/2 times its speed. You can make turns during this movement, but you add 2 to your starship’s distance between turns."
                    ]
                    [ text "Full Power (no check)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Full Power" ]
        , case ( model.phase, Status.audaciousGambit model.status { currentRound = model.roundNumber, starship = starship, useComputerNode = model.useComputerNode } ) of
            ( CP Piloting, Just ( _, bonus ) ) ->
                button
                    [ E.onClick AudaciousGambit
                    , A.title "If you have at least 12 ranks in Piloting, you can spend 1 Resolve Point and attempt a Piloting check (DC = 20 + 1-1/2 × your starship’s tier) to pull off complex maneuvers. You can move your starship up to its speed, treating its distance between turns as if it were 2 lower (minimum 0). You can also fly through hexes occupied by enemy vessels without provoking free attacks. At the end of your starship’s movement, you can rotate your starship to face in any direction. If you fail the check, you instead move as if you had taken the fly action (but still lose the Resolve Point)."
                    ]
                    [ text ("Audacious Gambit (" ++ String.fromInt bonus ++ " vs DC " ++ String.fromInt (20 + shipDCMod) ++ ")") ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Audacious Gambit" ]
        , case ( model.phase, Status.audaciousGambitFail model.status { currentRound = model.roundNumber, useComputerNode = model.useComputerNode, starship = starship } ) of
            ( CP Piloting, Just _ ) ->
                button
                    [ E.onClick AudaciousGambitFail ]
                    [ text "Audacious Gambit (Fail)" ]

            _ ->
                button
                    [ A.disabled True ]
                    [ text "Audacious Gambit (Fail)" ]
        , button
            [ case ( model.partialState, model.phase ) of
                ( Selected arc, CP Piloting ) ->
                    case Status.canBalanceFromTo arc model.status.shields of
                        [] ->
                            A.disabled True

                        _ ->
                            E.onClick StartBalanceFromArc

                _ ->
                    A.disabled True
            , A.title "You can balance the shields, redirecting power from one quadrant to protect another. With a successful Computers check (DC = 10 + 1-1/2 × your starship’s tier), you can shift Shield Points (SP) from the shield in one quadrant to the shield in another quadrant, including to depleted shields (after rebalancing, every shield must have at least 10% of the total current SP)."
            ]
            [ text ("Balance to other Shields (DC " ++ String.fromInt (10 + shipDCMod) ++ ")") ]
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
            [ case ( model.partialState, model.phase ) of
                ( None, CP Piloting ) ->
                    E.onClick BalanceEvenly

                _ ->
                    A.disabled True
            , A.title "You can balance the shields, redirecting power from one quadrant to protect another. With a successful Computers check (DC = 10 + 1-1/2 × your starship’s tier), you can add up the SP from all the remaining shields and evenly distribute them to all four quadrants, putting any excess SP in the forward quadrant."
            ]
            [ text ("Balance Shields Evenly (DC " ++ String.fromInt (10 + shipDCMod) ++ ")") ]
        , button
            [ case ( model.partialState, Status.maxDivertPowerToShieldPoints starship model.status <= 0, model.phase ) of
                ( None, False, CP Engineering ) ->
                    E.onClick StartDivertToShields

                _ ->
                    A.disabled True
            , A.title "You can divert auxiliary power into one of your starship’s systems, giving it a boost. This requires a successful Engineering check (DC = 10 + 1-1/2 × your starship’s tier), and the results depend on where you decide to send this extra power. If you send it to the shields, restore an amount of Shield Points equal to 5% of the PCU rating of the starship’s power core (see page 296), up to the shields’ maximum value. You can distribute the restored Shield Points across the shields’ four quadrants as you see fit."
            ]
            [ text ("Divert Power to Shields (DC " ++ String.fromInt (10 + shipDCMod) ++ ")") ]
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
            [ case ( model.partialState, model.phase ) of
                ( None, CP Engineering ) ->
                    E.onClick DivertToEngines

                _ ->
                    A.disabled True
            , A.title "You can divert auxiliary power into one of your starship’s systems, giving it a boost. This requires a successful Engineering check (DC = 10 + 1-1/2 × your starship’s tier), and the results depend on where you decide to send this extra power. If you send it to the engines, your starship’s speed increases by 2 this round."
            ]
            [ text ("Divert Power to Engines (DC " ++ String.fromInt (10 + shipDCMod) ++ ")") ]
        , button
            [ E.onClick AcceptAllotmentToShields, A.disabled (not allotmentIsValid) ]
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
                            { currentRound = model.roundNumber, currentPhase = combatPhase, starship = starship, useComputerNode = model.useComputerNode }

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
        , div [] [ text "-----" ]
        , div [] <|
            Arc.foldWithAnArc
                (\anArc weapons divs ->
                    let
                        pre =
                            case anArc of
                                Forward ->
                                    "Forward: "

                                Aft ->
                                    "Aft: "

                                Starboard ->
                                    "Starboard: "

                                Port ->
                                    "Port: "
                    in
                    List.map
                        (\w ->
                            div []
                                [ text (pre ++ WeaponDescription.view w)
                                ]
                        )
                        weapons
                        ++ divs
                )
                []
                starship.arcWeapons

        -- TODO: All weapons should probably be combined in a single foldable
        -- type with deeper indexing
        , div [] <|
            List.map
                (\w ->
                    div []
                        [ text ("Turret: " ++ WeaponDescription.view w)
                        ]
                )
                starship.turretWeapons
        , div [] [ text "-----" ]
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
            [ E.onClick NextPhase, A.disabled (maybeAllotting model.partialState /= Nothing) ]
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
