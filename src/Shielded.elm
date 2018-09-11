module Shielded exposing (Shielded, view)

import Arc exposing (AnArc(..))
import Color exposing (Color, blue, green, grey, red, yellow)
import ShieldArc
import Svg exposing (Svg)
import Svg.Attributes as SA


type alias Shielded b a =
    { size : Float
    , arcColors : Arc.Arc Color
    , arcOnClick : AnArc -> a
    , shielded : b
    }


view : (Float -> b -> Svg a) -> Shielded b a -> Svg a
view shielded model =
    let
        arcModel =
            { radius = model.size / 2
            , colors = model.arcColors
            , onClick = model.arcOnClick
            }

        shieldedSize =
            ShieldArc.radiusToInnerSize (model.size / 2)

        shieldedOffset =
            (model.size - shieldedSize) / 2
    in
    Svg.g []
        [ ShieldArc.view arcModel
        , Svg.g
            [ SA.transform <|
                "translate("
                    ++ String.fromFloat shieldedOffset
                    ++ ","
                    ++ String.fromFloat shieldedOffset
                    ++ ")"
            ]
            [ shielded shieldedSize model.shielded
            ]
        ]
