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


type alias Model =
    { status : Status
    , critsRemaining : Int
    , selected : Maybe AnArc
    , damageInput : Maybe Int
    , diverting : Maybe (Arc.Arc Int)
    }


init : Starship -> Model
init starship =
    { status =
        { damage =
            0

        -- TODO: Users determine this as they go into combat
        , shields = Arc.pure ((extract starship.shields).shieldPoints // 4)
        , lifeSupport = Nothing
        , sensors = Nothing
        , weaponsArray = Arc.pure Nothing
        , engines = Nothing
        , powerCore = Nothing
        }
    , critsRemaining = 0
    , selected = Nothing
    , damageInput = Nothing
    , diverting = Nothing
    }


type Msg
    = Damage AnArc Int Bool
    | ApplyCrit Status.PatchableSystem
    | SelectSheildArc AnArc
    | DeselectSheildArc
    | ChangeDamageInput (Maybe Int)
    | StartDivertToShields
    | EditDivertToShields AnArc (Int -> Int)
    | CancelDivertToShields
    | AcceptDivertToShields
    | BalanceToAllFrom AnArc Int
    | BalanceEvenly
    | Patch PatchableSystem
    | HoldItTogether PatchableSystem
    | QuickFix PatchableSystem
    | NextRound


update : Starship -> Msg -> Model -> ( Model, Cmd Msg )
update starship msg model =
    -- TODO: Restrict actions based on the current phase of the round
    case msg of
        Damage arc damage wasCrit ->
            -- This may be over defensive, but we lock the model
            -- while we are waiting for the random result to come back
            if model.critsRemaining == 0 then
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
                    , selected = Nothing
                  }
                , Cmd.batch
                    (List.repeat critCount
                        (Random.generate ApplyCrit
                            Status.pickPatchableSystem
                        )
                    )
                )

            else
                ( model, Cmd.none )

        ApplyCrit system ->
            ( { model
                | status = Status.damageSystem Nothing system model.status
                , critsRemaining = model.critsRemaining - 1
              }
            , Cmd.none
            )

        SelectSheildArc arc ->
            -- Shields are unselectable when the ship is dead
            -- Shields are unselectable when diverting power
            if getDamagePercent starship model.status > 0 && model.diverting == Nothing then
                ( { model | selected = Just arc }, Cmd.none )

            else
                ( model, Cmd.none )

        DeselectSheildArc ->
            ( { model | selected = Nothing }, Cmd.none )

        ChangeDamageInput (Just x) ->
            if x > 0 then
                ( { model | damageInput = Just x }, Cmd.none )

            else
                ( model, Cmd.none )

        ChangeDamageInput Nothing ->
            ( { model | damageInput = Nothing }, Cmd.none )

        StartDivertToShields ->
            ( { model | diverting = Just (Arc.pure 0) }, Cmd.none )

        EditDivertToShields arc f ->
            ( { model
                | diverting =
                    Maybe.map (Arc.updateArc f arc) model.diverting
              }
            , Cmd.none
            )

        CancelDivertToShields ->
            ( { model | diverting = Nothing }, Cmd.none )

        AcceptDivertToShields ->
            ( model.diverting
                |> Maybe.andThen
                    (\added ->
                        Status.divertPowerToShields starship added model.status
                    )
                |> Maybe.map
                    (\newStatus ->
                        { model | diverting = Nothing, status = newStatus }
                    )
                |> Maybe.withDefault model
            , Cmd.none
            )

        BalanceToAllFrom arc amount ->
            case Status.balanceToAll starship arc amount model.status of
                Just status ->
                    ( { model | status = status, selected = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        BalanceEvenly ->
            ( { model | status = Status.balanceEvenly starship model.status }, Cmd.none )

        Patch patchableSystem ->
            ( { model | status = Status.patchStatus patchableSystem model.status }, Cmd.none )

        HoldItTogether patchableSystem ->
            ( { model | status = Status.holdItTogether patchableSystem model.status }, Cmd.none )

        QuickFix patchableSystem ->
            ( { model | status = Status.quickFix patchableSystem model.status }, Cmd.none )

        NextRound ->
            ( { model | status = Status.tick model.status }, Cmd.none )


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


criticalStatusToRgb : Maybe CriticalStatus -> Color
criticalStatusToRgb =
    Maybe.andThen Status.getEffectiveCriticalStatus
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
            { status | shields = Arc.liftA2 (+) status.shields added }

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


view : Starship -> Model -> Html Msg
view starship model =
    let
        size =
            200

        sizeStr =
            String.fromFloat size

        damagePercent =
            getDamagePercent starship model.status

        patchableDisplay name status patchableSystem =
            let
                impacted =
                    Maybe.andThen Status.getEffectiveCriticalStatus status == Nothing
            in
            div
                [ A.style
                    "background-color"
                    (colorToCssRgb <|
                        if damagePercent > 0 then
                            criticalStatusToRgb status

                        else
                            grey
                    )
                ]
                [ button
                    [ A.disabled impacted
                    , A.title "PATCH: repair one level of severity."
                    , E.onClick (Patch patchableSystem)
                    ]
                    [ text "P" ]
                , button
                    [ A.disabled impacted
                    , A.title "HOLD IT TOGETHER: temporarily repair two levels of severtity for a single round."
                    , E.onClick (HoldItTogether patchableSystem)
                    ]
                    [ text "H" ]
                , button
                    [ A.disabled impacted
                    , A.title "QUICK FIX (1RP): Completely ignore all critical damage for this system for 1 hour."
                    , E.onClick (QuickFix patchableSystem)
                    ]
                    [ text "Q" ]
                , text name
                ]
    in
    div []
        [ Svg.svg
            [ SA.height sizeStr
            , SA.width sizeStr
            , SA.viewBox <| "0 0 " ++ sizeStr ++ " " ++ sizeStr
            ]
            [ case ( model.selected, model.diverting ) of
                ( Nothing, Nothing ) ->
                    shieldedFighter starship model.status SelectSheildArc (Arc.pure Nothing) (Arc.pure Nothing) size

                ( Just selected, _ ) ->
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

                ( _, Just added ) ->
                    divertingShieldedFighter starship model.status added size
            ]
        , input
            [ A.value (Maybe.map String.fromInt model.damageInput |> Maybe.withDefault "")
            , A.disabled (model.selected == Nothing)
            , E.onInput (String.toInt >> ChangeDamageInput)
            , A.type_ "number"
            ]
            []
        , button
            (case ( model.selected, model.damageInput ) of
                ( Just arc, Just damageInput ) ->
                    [ E.onClick (Damage arc damageInput True) ]

                _ ->
                    [ A.disabled True ]
            )
            [ text "Damage w/Crit" ]
        , button
            (case ( model.selected, model.damageInput ) of
                ( Just arc, Just damageInput ) ->
                    [ E.onClick (Damage arc damageInput False) ]

                _ ->
                    [ A.disabled True ]
            )
            [ text "Damage" ]
        , button
            (case ( model.selected, model.damageInput ) of
                ( Just arc, Just damageInput ) ->
                    [ A.disabled
                        (Status.balanceToAll starship arc damageInput model.status == Nothing)
                    , E.onClick
                        (BalanceToAllFrom arc damageInput)
                    ]

                _ ->
                    [ A.disabled True ]
            )
            [ text "Balance To All Others" ]
        , button
            (case ( model.selected, model.diverting ) of
                ( Nothing, Nothing ) ->
                    [ E.onClick BalanceEvenly ]

                _ ->
                    [ A.disabled True ]
            )
            [ text "Balance Shields Evenly" ]
        , button
            (case ( model.selected, model.diverting, Status.maxDivertPowerToShieldPoints starship model.status <= 0 ) of
                ( Nothing, Nothing, False ) ->
                    [ E.onClick StartDivertToShields ]

                _ ->
                    [ A.disabled True ]
            )
            [ text "Divert Power to Shields" ]
        , button
            (case model.diverting of
                Nothing ->
                    [ A.disabled True ]

                Just arc ->
                    [ E.onClick CancelDivertToShields ]
            )
            [ text "Cancel Divert Power to Shields" ]
        , button
            (case Maybe.andThen (\x -> Status.divertPowerToShields starship x model.status) model.diverting of
                Nothing ->
                    [ A.disabled True ]

                Just arc ->
                    [ E.onClick AcceptDivertToShields ]
            )
            [ text "Accept Divert Power to Shields" ]

        -- TODO: Apply a temporary status to patchable system
        , patchableDisplay "Life Support" model.status.lifeSupport LifeSupport
        , patchableDisplay "Sensors" model.status.sensors Sensors
        , patchableDisplay "Weapons Array - Forward" model.status.weaponsArray.forward (WeaponsArray Arc.Forward)
        , patchableDisplay "Weapons Array - Aft" model.status.weaponsArray.aft (WeaponsArray Arc.Aft)
        , patchableDisplay "Weapons Array - Port" model.status.weaponsArray.portSide (WeaponsArray Arc.Port)
        , patchableDisplay "Weapons Array - Starboard" model.status.weaponsArray.starboard (WeaponsArray Arc.Starboard)
        , patchableDisplay "Engines" model.status.engines Engines
        , patchableDisplay "Power Core" model.status.powerCore PowerCore
        , button
            [ E.onClick NextRound ]
            [ text "NEXT ROUND" ]
        ]


main : Program () Model Msg
main =
    element
        { init = \_ -> ( init norikamaDropship, Cmd.none )
        , update = update norikamaDropship
        , view = view norikamaDropship
        , subscriptions = \_ -> Sub.none
        }
