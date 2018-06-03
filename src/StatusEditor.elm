module StatusEditor exposing (..)

import Status exposing (Status, PatchableSystem(..))
import Starship exposing (Starship)
import Arc exposing (AnArc(..))
import Html exposing (Html, div, button, text, beginnerProgram)
import Html.Attributes as A
import Html.Events as E
import Color exposing (Color, red, green, yellow)
import Color.Manipulate exposing (weightedMix)
import Color.Convert exposing (colorToCssRgb)
import Togglable exposing (extract)
import ShipAssets exposing (..)


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
                        model.starship.frame.criticalThreshold
                        criticalSystem
                        arc
                        damage
                        model.status
            }


colorTransition : Float -> Color
colorTransition x =
    if x > 0.5 then
        weightedMix green yellow (2 * (x - 0.5))
    else
        weightedMix yellow red (2 * x)


view : Model -> Html Msg
view model =
    let
        hp =
            Starship.getMaxHitPoints model.starship

        damagePercent =
            toFloat (hp - model.status.damage) / toFloat hp

        forwardDamagePercent =
            toFloat model.status.shields.forward
                -- TODO:  Need to handle disabled shields
                /
                    (toFloat (extract model.starship.shields).shieldPoints / 4)
    in
        div []
            [ div
                [ A.style
                    [ ( "height", "20px" )
                    , ( "width", "20px" )
                    , ( "background-color"
                      , colorToCssRgb (colorTransition forwardDamagePercent)
                      )
                    ]
                ]
                []
            , div
                [ A.style
                    [ ( "height", "30px" )
                    , ( "width", "30px" )
                    , ( "background-color"
                      , colorToCssRgb (colorTransition damagePercent)
                      )
                    ]
                ]
                []
              -- TODO: Damage needs to be input-able
            , button [ E.onClick (Damage LifeSupport Forward 3) ] [ text "Damage" ]
            ]


main : Program Never Model Msg
main =
    beginnerProgram { model = init, update = update, view = view }
