module ShieldArc exposing (Model, innerSizeToRadius, radiusToInnerSize, view)

import Arc exposing (AnArc(..))
import Browser exposing (sandbox)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)
import Html exposing (Html, button, div, text)
import Svg exposing (Svg)
import Svg.Attributes as SA
import SvgUtils as SU exposing (Segment(..), Strategy(..))


type alias SingleModel =
    { radius : Float
    , color : Color
    , rotation : Float
    }


innerSizeToRadius : Float -> Float
innerSizeToRadius size =
    size * sqrt 2 / 2


radiusToInnerSize : Float -> Float
radiusToInnerSize radius =
    2 * radius / sqrt 2


single : SingleModel -> Svg a
single model =
    let
        innerSize =
            radiusToInnerSize model.radius

        depth =
            model.radius - innerSize / 2
    in
    Svg.path
        [ SA.fill (colorToCssRgb model.color)
        , SA.transform <|
            "rotate("
                ++ String.fromFloat model.rotation
                ++ ","
                ++ String.fromFloat model.radius
                ++ ","
                ++ String.fromFloat model.radius
                ++ ")"
        , SU.d
            ( True
            , [ ( Absolute, MoveTo ( depth, depth ) )
              , ( Relative, Arc ( model.radius, model.radius ) 0 False True ( innerSize, 0 ) )
              , ( Relative, Arc ( innerSize, innerSize ) 0 False False ( -innerSize, 0 ) )
              ]
            )
        ]
        []


type alias Model =
    { radius : Float
    , colors : Arc.Arc Color
    }


view : Model -> Svg a
view model =
    Svg.g
        []
        (Arc.foldWithAnArc
            (\arc color list ->
                single
                    { radius = model.radius
                    , rotation = Arc.getDegrees arc
                    , color = color
                    }
                    :: list
            )
            []
            model.colors
        )



--TODO: Move to examples


main : Program () Model a
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
            }
        , update = always identity
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
