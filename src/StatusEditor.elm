module StatusEditor exposing (Model, Msg(..), colorTransition, criticalStatusToRgb, init, main, maybeSeverityToPercent, update, view)

import Arc exposing (AnArc(..))
import Browser exposing (element)
import Color exposing (Color, blue, green, grey, red, yellow)
import Color.Convert exposing (colorToCssRgb)
import Color.Manipulate exposing (weightedMix)
import CounterArc
import Fighter
import Html exposing (Html, button, div, input, text)
import Html.Attributes as A
import Html.Events as E
import Platform.Cmd exposing (Cmd)
import Platform.Sub
import Random
import ShieldArc
import Shielded
import ShipAssets exposing (..)
import Starship exposing (Starship)
import Status exposing (CriticalStatus, PatchableSystem(..), Severity(..), Status)
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
    = Engineering
    | Piloting
    | Gunnery


nextPhase : Phase -> Phase
nextPhase phase =
    case phase of
        Engineering ->
            Piloting

        Piloting ->
            Gunnery

        Gunnery ->
            Engineering


phaseToString : Phase -> String
phaseToString phase =
    case phase of
        Engineering ->
            "Engineering"

        Piloting ->
            "Piloting"

        Gunnery ->
            "Gunnery"


type alias Model =
    { status : Status
    , critsRemaining : Int
    , damageInput : Maybe Int
    , partialState : PartialState
    , roundNumber : Int
    , phase : Phase
    }


init : Starship -> Model
init starship =
    { status = Status.init
    , critsRemaining = 0
    , damageInput = Nothing
    , partialState = Allotting (Arc.pure ((extract starship.shields).shieldPoints // 4))
    , roundNumber = 0
    , phase = Engineering
    }


type Msg
    = Damage Int Bool
    | ApplyCrit Status.PatchableSystem
    | SelectSheildArc AnArc
    | DeselectSheildArc
    | ChangeDamageInput (Maybe Int)
    | StartDivertToShields
    | EditDivertToShields AnArc (Int -> Int)
    | CancelDivertToShields
    | AcceptDivertToShields
    | EditAllotmentToShields AnArc (Int -> Int)
    | AcceptAllotmentToShields
    | StartBalanceFromArc
    | EditBalanceFromArc AnArc Int
    | CancelBalanceFromArc
    | AcceptBalanceFromArc
    | BalanceEvenly
    | Patch Status.PatchEffectiveness PatchableSystem
    | HoldItTogether PatchableSystem
    | QuickFix PatchableSystem
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
                                Status.pickPatchableSystem
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
            ( { model | status = Status.patchStatus pe patchableSystem model.status }, Cmd.none )

        HoldItTogether patchableSystem ->
            ( { model | status = Status.holdItTogether model.roundNumber patchableSystem model.status }, Cmd.none )

        QuickFix patchableSystem ->
            ( { model | status = Status.quickFix patchableSystem model.status }, Cmd.none )

        NextPhase ->
            ( { model
                | roundNumber =
                    model.roundNumber
                        + (case model.phase of
                            Gunnery ->
                                1

                            _ ->
                                0
                          )
                , phase = nextPhase model.phase
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
    Maybe.andThen (Status.getEffectiveSeverity roundNumber)
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
                    Maybe.andThen (Status.getEffectiveSeverity model.roundNumber) status

                impacted =
                    effectiveSeverity == Nothing

                xOfYStr x y =
                    "(" ++ String.fromInt x ++ "/" ++ String.fromInt y ++ ")"

                patchesNeeded severity =
                    case severity of
                        Glitching ->
                            1

                        Malfunctioning ->
                            2

                        Wrecked ->
                            3

                patchDisplay =
                    status
                        |> Maybe.map Status.patchCount
                        |> Maybe.withDefault 0
                        |> (\n -> Maybe.map (patchesNeeded >> xOfYStr n) effectiveSeverity)
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
                    [ A.disabled (Maybe.andThen (Status.basePatchDC Status.Single) status == Nothing || not isEngineeringPhase)
                    , A.title "PATCH: apply a single patch towards the repair of one level of severity."
                    , E.onClick (Patch Status.Single patchableSystem)
                    ]
                    [ text "P" ]
                , button
                    [ A.disabled (Maybe.andThen (Status.basePatchDC Status.Double) status == Nothing || not isEngineeringPhase)
                    , A.title "PATCH: apply two patches towards the repair of one level of severity."
                    , E.onClick (Patch Status.Double patchableSystem)
                    ]
                    [ text "Px2" ]
                , button
                    [ A.disabled (Maybe.andThen (Status.basePatchDC Status.Triple) status == Nothing || not isEngineeringPhase)
                    , A.title "PATCH: apply three patches towards the repair of one level of severity."
                    , E.onClick (Patch Status.Triple patchableSystem)
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
        , input
            [ A.value (Maybe.map String.fromInt model.damageInput |> Maybe.withDefault "")
            , A.disabled
                (case ( model.partialState, model.phase ) of
                    ( Selected _, Gunnery ) ->
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
                ( Selected arc, Just damageInput, Gunnery ) ->
                    [ E.onClick (Damage damageInput True) ]

                _ ->
                    [ A.disabled True ]
            )
            [ text "Damage w/Crit" ]
        , button
            (case ( model.partialState, model.damageInput, model.phase ) of
                ( Selected arc, Just damageInput, Gunnery ) ->
                    [ E.onClick (Damage damageInput False) ]

                _ ->
                    [ A.disabled True ]
            )
            [ text "Damage" ]
        , button
            (case ( model.partialState, model.phase ) of
                ( Selected arc, Piloting ) ->
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
                ( None, Piloting ) ->
                    [ E.onClick BalanceEvenly ]

                _ ->
                    [ A.disabled True ]
            )
            [ text "Balance Shields Evenly" ]
        , button
            (case ( model.partialState, Status.maxDivertPowerToShieldPoints starship model.status <= 0, model.phase ) of
                ( None, False, Engineering ) ->
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
            [ E.onClick AcceptAllotmentToShields, A.disabled (not isStillAllotting) ]
            [ text "Accept Allotted Shields" ]
        , patchableDisplay (model.phase == Engineering) "Life Support" model.status.lifeSupport LifeSupport
        , patchableDisplay (model.phase == Engineering) "Sensors" model.status.sensors Sensors
        , patchableDisplay (model.phase == Engineering) "Weapons Array - Forward" model.status.weaponsArray.forward (WeaponsArray Arc.Forward)
        , patchableDisplay (model.phase == Engineering) "Weapons Array - Aft" model.status.weaponsArray.aft (WeaponsArray Arc.Aft)
        , patchableDisplay (model.phase == Engineering) "Weapons Array - Port" model.status.weaponsArray.portSide (WeaponsArray Arc.Port)
        , patchableDisplay (model.phase == Engineering) "Weapons Array - Starboard" model.status.weaponsArray.starboard (WeaponsArray Arc.Starboard)
        , patchableDisplay (model.phase == Engineering) "Engines" model.status.engines Engines
        , patchableDisplay (model.phase == Engineering) "Power Core" model.status.powerCore PowerCore
        , button
            [ E.onClick NextPhase, A.disabled isStillAllotting ]
            [ text ("PROCEED TO " ++ String.toUpper (phaseToString (nextPhase model.phase)) ++ " PHASE") ]
        ]


main : Program () Model Msg
main =
    element
        { init = \_ -> ( init norikamaDropship, Cmd.none )
        , update = update norikamaDropship
        , view = view norikamaDropship
        , subscriptions = \_ -> Sub.none
        }
