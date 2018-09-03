module Main exposing (main)

import Arc exposing (AnArc)
import Browser exposing (sandbox)
import Color
import Html exposing (div)
import ShieldArc exposing (Model, view)
import Svg exposing (Svg)
import Svg.Attributes as SA


main : Program () (Model AnArc) AnArc
main =
    sandbox
        { init =
            { radius = 200
            , colors =
                { forward = Color.green
                , portSide = Color.green
                , aft = Color.green
                , starboard = Color.blue
                }
            , onClick = identity
            }
        , update = \msg model -> (\( _, x ) -> x) ( Debug.log "Arc Clicked" msg, model )
        , view =
            \model ->
                let
                    heightStr =
                        String.fromFloat (model.radius * 2)

                    widthStr =
                        String.fromFloat (model.radius * 2)
                in
                div []
                    [ Svg.svg
                        [ SA.height heightStr
                        , SA.width widthStr
                        , SA.viewBox ("0 0 " ++ widthStr ++ " " ++ heightStr)
                        ]
                        [ view model
                        ]
                    ]
        }
