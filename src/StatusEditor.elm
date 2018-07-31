module StatusEditor exposing (..)

import Switch exposing (Switch(..))
import Status exposing (Status, PatchableSystem(..))
import Starship exposing (Starship)
import Arc exposing (AnArc(..))
import Html exposing (Html, div, button, text, beginnerProgram)
import Html.Attributes as A
import Html.Events as E
import Color exposing (Color, red, green, yellow, grey)
import Color.Manipulate exposing (weightedMix)
import Togglable exposing (extract, meta)
import ShipAssets exposing (..)
import ShieldArc
import Fighter


type alias Model =
    { status : Status
    , starship : Starship
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
    }


type
    Msg
    -- TODO: The patchable system needs to be chosen at random based on rulebook table
    = Damage PatchableSystem AnArc Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Damage criticalSystem arc damage ->
            { model
                | status =
                    Status.damageArc
                        criticalSystem
                        model.starship
                        arc
                        damage
                        model.status
            }


colorTransition : Float -> Color
colorTransition x =
    if x > 0.5 then
        weightedMix green yellow (2 * (x - 0.5))
    else if x > 0 then
        weightedMix yellow red (2 * x)
    else
        grey


view : Model -> Html Msg
view model =
    let
        hp =
            Starship.getMaxHitPoints model.starship

        damagePercent =
            toFloat (hp - model.status.damage) / toFloat hp

        shieldDamagePercent arc =
            let
                points =
                    case arc of
                        Forward ->
                            model.status.shields.forward

                        Port ->
                            model.status.shields.portSide

                        Starboard ->
                            model.status.shields.starboard

                        Aft ->
                            model.status.shields.aft
            in
                if damagePercent > 0 && meta model.starship.shields == On then
                    toFloat points
                        / (toFloat (extract model.starship.shields).shieldPoints / 4)
                else
                    0

        rowStyle =
            A.style [ ( "display", "flex" ), ( "flex-direction", "row" ) ]

        spacer =
            div [ A.style [ ( "width", toString (ShieldArc.getHeight 150) ++ "px" ) ] ] []

        -- TODO: See the note in the ShieldArc module, this strategy has led down
        -- a more complex path than anticipated, and what was originally intended
        -- as pure SVG is now heavily dependent on div/styles for positioning.
        -- Things like dependence on ShieldArc.getHeight show abstraction leaks.
        mkArc arc =
            ShieldArc.asHtml
                { arc = arc
                , size = 150
                , color = colorTransition (shieldDamagePercent arc)
                }
    in
        div []
            [ div [ rowStyle ]
                [ spacer
                , mkArc Forward
                ]
            , div [ rowStyle ]
                [ mkArc Port
                , Fighter.asHtml { size = 150, color = colorTransition damagePercent }
                , mkArc Starboard
                ]
            , div [ rowStyle ]
                [ spacer
                , mkArc Aft
                ]
              -- TODO: Damage needs to be input-able
            , button [ E.onClick (Damage LifeSupport Forward 3) ] [ text "Damage" ]
            ]


main : Program Never Model Msg
main =
    beginnerProgram { model = init, update = update, view = view }
