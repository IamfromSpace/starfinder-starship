module StatusEditor exposing (Model, Msg(..), colorTransition, criticalStatusToRgb, init, main, maybeSeverityToPercent, update, view)

import Arc exposing (AnArc(..))
import Browser exposing (element)
import Color exposing (Color, blue, green, grey, red, yellow)
import Color.Convert exposing (colorToCssRgb)
import Color.Manipulate exposing (weightedMix)
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
    , damageInput : Int
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
    , damageInput = 1
    }


type Msg
    = Damage AnArc Int Bool
    | ApplyCrit Status.PatchableSystem
    | SelectSheildArc AnArc
    | DeselectSheildArc
    | ChangeDamageInput Int
    | BalanceToAllFrom AnArc Int
    | BalanceEvenly


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
            if getDamagePercent starship model.status > 0 then
                ( { model | selected = Just arc }, Cmd.none )

            else
                ( model, Cmd.none )

        DeselectSheildArc ->
            ( { model | selected = Nothing }, Cmd.none )

        ChangeDamageInput x ->
            if x > 0 then
                ( { model | damageInput = x }, Cmd.none )

            else
                ( model, Cmd.none )

        BalanceToAllFrom arc amount ->
            case Status.balanceToAll starship arc amount model.status of
                Just status ->
                    ( { model | status = status, selected = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        BalanceEvenly ->
            ( { model | status = Status.balanceEvenly starship model.status }, Cmd.none )


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


view : Starship -> Model -> Html Msg
view starship model =
    let
        size =
            200

        sizeStr =
            String.fromFloat size

        damagePercent =
            getDamagePercent starship model.status

        shieldDamagePercents =
            Arc.map
                (\points ->
                    if damagePercent > 0 && meta starship.shields == On then
                        toFloat points
                            / (toFloat (extract starship.shields).shieldPoints / 4)

                    else
                        0
                )
                model.status.shields

        shipAndShieldsBase =
            { size = size
            , arcColors = Arc.map colorTransition shieldDamagePercents
            , arcOnClick = SelectSheildArc
            , shielded = colorTransition damagePercent
            }

        selectedArc a =
            { size = size
            , arcColors =
                Arc.mapWithAnArc
                    (\arc _ ->
                        if arc == a then
                            Color.black

                        else
                            Color.grey
                    )
                    (Arc.pure ())
            , arcOnClick =
                \arc ->
                    -- Clear the selection if this is the selected arc
                    if arc == a then
                        DeselectSheildArc

                    else
                        SelectSheildArc arc
            , shielded = colorTransition damagePercent
            }

        patchableDisplay name status =
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
                [ text name ]
    in
    div []
        [ Svg.svg
            [ SA.height sizeStr
            , SA.width sizeStr
            , SA.viewBox <| "0 0 " ++ sizeStr ++ " " ++ sizeStr
            ]
            [ Shielded.view
                (\size_ color ->
                    Fighter.view
                        { size = size_
                        , color = color
                        }
                )
                (model.selected
                    |> Maybe.map selectedArc
                    |> Maybe.withDefault shipAndShieldsBase
                )
            ]
        , input
            [ A.value (String.fromInt model.damageInput)
            , A.disabled (model.selected == Nothing)
            , E.onInput (String.toInt >> Maybe.withDefault 1 >> ChangeDamageInput)
            , A.type_ "number"
            ]
            []
        , button
            (case model.selected of
                Nothing ->
                    [ A.disabled True ]

                Just arc ->
                    [ E.onClick (Damage arc model.damageInput True) ]
            )
            [ text "Damage w/Crit" ]
        , button
            (case model.selected of
                Nothing ->
                    [ A.disabled True ]

                Just arc ->
                    [ E.onClick (Damage arc model.damageInput False) ]
            )
            [ text "Damage" ]
        , button
            (case model.selected of
                Nothing ->
                    [ A.disabled True ]

                Just arc ->
                    [ A.disabled
                        (Status.balanceToAll starship arc model.damageInput model.status == Nothing)
                    , E.onClick
                        (BalanceToAllFrom arc model.damageInput)
                    ]
            )
            [ text "Balance To All Others" ]
        , button
            (case model.selected of
                Nothing ->
                    [ E.onClick BalanceEvenly ]

                Just arc ->
                    [ A.disabled True ]
            )
            [ text "Balance Shields Evenly" ]

        -- TODO: patch a patchable system
        -- TODO: hold together a patchable system
        -- TODO: quick fix a patchable system
        -- TODO: Apply a temporary status to patchable system
        , patchableDisplay "Life Support" model.status.lifeSupport
        , patchableDisplay "Sensors" model.status.sensors
        , patchableDisplay "Weapons Array - Forward" model.status.weaponsArray.forward
        , patchableDisplay "Weapons Array - Aft" model.status.weaponsArray.aft
        , patchableDisplay "Weapons Array - Port" model.status.weaponsArray.portSide
        , patchableDisplay "Weapons Array - Starboard" model.status.weaponsArray.starboard
        , patchableDisplay "Engines" model.status.engines
        , patchableDisplay "Power Core" model.status.powerCore
        ]


main : Program () Model Msg
main =
    element
        { init = \_ -> ( init norikamaDropship, Cmd.none )
        , update = update norikamaDropship
        , view = view norikamaDropship
        , subscriptions = \_ -> Sub.none
        }
