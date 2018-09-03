module StatusEditor exposing (Model, Msg(..), colorTransition, criticalStatusToRgb, init, main, maybeSeverityToPercent, update, view)

import Arc exposing (AnArc(..))
import Browser exposing (element)
import Color exposing (Color, green, grey, red, yellow)
import Color.Convert exposing (colorToCssRgb)
import Color.Manipulate exposing (weightedMix)
import Fighter
import Html exposing (Html, button, div, text)
import Html.Attributes as A
import Html.Events as E
import Platform.Cmd exposing (Cmd)
import Platform.Sub
import Random
import ShieldArc
import ShipAssets exposing (..)
import Starship exposing (Starship)
import Status exposing (CriticalStatus, PatchableSystem(..), Severity(..), Status)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Switch exposing (Switch(..))
import Togglable exposing (extract, meta)


type alias Model =
    { status : Status
    , starship : Starship
    , locked : Bool
    , selected : Maybe AnArc
    }


init : Model
init =
    { status =
        { damage =
            0

        -- TODO: Users determine this as they go into combat
        , shields = Arc.pure ((extract norikamaDropship.shields).shieldPoints // 4)
        , lifeSupport = Nothing
        , sensors = Nothing
        , weaponsArray = Arc.pure Nothing
        , engines = Nothing
        , powerCore = Nothing
        }

    -- TODO: Obviously this should be injectable, not pre-defined
    , starship = norikamaDropship
    , locked = False
    , selected = Nothing
    }


type Msg
    = Damage AnArc Int
    | ApplyDamage Status
    | SelectSheildArc AnArc
    | DeselectSheildArc


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- TODO: Restrict actions based on the current phase of the round
    case msg of
        Damage arc damage ->
            -- This may be over defensive, but we lock the model
            -- while we are waiting for the random result to come back
            if not model.locked then
                ( { model | locked = True }
                , Random.generate
                    (\criticalSystem ->
                        ApplyDamage <|
                            Status.damageArc
                                criticalSystem
                                model.starship
                                arc
                                damage
                                model.status
                    )
                    Status.pickPatchableSystem
                )

            else
                ( model, Cmd.none )

        ApplyDamage status ->
            if model.locked then
                ( { model | status = status, locked = False, selected = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        SelectSheildArc arc ->
            -- Shields are unselectable when the ship is dead
            if getDamagePercent model.starship model.status > 0 then
                ( { model | selected = Just arc }, Cmd.none )

            else
                ( model, Cmd.none )

        DeselectSheildArc ->
            ( { model | selected = Nothing }, Cmd.none )


colorTransition : Float -> Color
colorTransition x =
    if x > 0.5 then
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


view : Model -> Html Msg
view model =
    let
        size =
            200

        sizeStr =
            String.fromFloat size

        damagePercent =
            getDamagePercent model.starship model.status

        shieldDamagePercents =
            Arc.map
                (\points ->
                    if damagePercent > 0 && meta model.starship.shields == On then
                        toFloat points
                            / (toFloat (extract model.starship.shields).shieldPoints / 4)

                    else
                        0
                )
                model.status.shields

        arcModel =
            { radius = size / 2
            , colors =
                case model.selected of
                    Nothing ->
                        Arc.map colorTransition shieldDamagePercents

                    Just a ->
                        Arc.mapWithAnArc
                            (\arc _ ->
                                if arc == a then
                                    Color.blue

                                else
                                    Color.grey
                            )
                            (Arc.pure ())
            , onClick =
                case model.selected of
                    Nothing ->
                        SelectSheildArc

                    _ ->
                        \_ -> DeselectSheildArc
            }

        shipSize =
            ShieldArc.radiusToInnerSize (size / 2)

        shipOffset =
            (size - shipSize) / 2

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
            [ ShieldArc.view arcModel
            , Svg.g
                [ SA.transform <|
                    "translate("
                        ++ String.fromFloat shipOffset
                        ++ ","
                        ++ String.fromFloat shipOffset
                        ++ ")"
                ]
                [ Fighter.view
                    { size = shipSize
                    , color = colorTransition damagePercent
                    }
                ]
            ]

        -- TODO: Damage points should be based on user input
        , button
            (case model.selected of
                Nothing ->
                    [ A.disabled True ]

                Just arc ->
                    [ E.onClick (Damage arc 14) ]
            )
            [ text "Damage" ]

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
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
