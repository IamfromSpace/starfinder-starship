module CounterArc exposing (Model, view)

import Arc exposing (AnArc(..))
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgba)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import SvgUtils as SU exposing (Segment(..), Strategy(..))


translate : ( Float, Float ) -> String
translate ( x, y ) =
    "translate(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ")"


type alias SingleModel a =
    { count : Int
    , size : Float
    , isHorizontal : Bool
    , offset : Float
    , color : Color
    , backgroundColor : Color
    , onPlus : Maybe a
    , onMinus : Maybe a
    }


single : SingleModel a -> Svg a
single model =
    let
        invisible =
            Color.rgba 1 1 1 0

        textCharWidth x =
            if x == 0 then
                1

            else
                toFloat x
                    |> abs
                    |> logBase 10
                    |> floor
                    |> (+)
                        (if x < 0 then
                            2

                         else
                            1
                        )

        hButtonGap =
            model.count
                |> textCharWidth
                |> toFloat
                |> (+) 2
                |> (*) model.size
                |> (*) 0.6

        vButtonGap =
            model.size * 1.6

        vFontSize =
            model.count
                |> textCharWidth
                |> toFloat
                |> (/) model.size
                |> (*) (1 / 0.6)
                |> min model.size

        fontSize =
            if model.isHorizontal then
                model.size

            else
                vFontSize

        rect hasStroke mOnClick fill isLeft =
            let
                strokeWidth =
                    model.size / 14

                dir =
                    if isLeft then
                        -1

                    else
                        1

                withOnClick =
                    case mOnClick of
                        Just msg ->
                            (::) (SE.onClick msg)

                        Nothing ->
                            identity
            in
            Svg.rect
                (withOnClick
                    [ SA.fill (colorToCssRgba fill)
                    , SA.stroke <| colorToCssRgba model.color
                    , SA.strokeWidth <| String.fromFloat strokeWidth
                    , SA.height <| String.fromFloat (model.size - strokeWidth)
                    , SA.width <| String.fromFloat (model.size - strokeWidth)
                    , SA.transform <|
                        translate <|
                            if model.isHorizontal then
                                ( -model.size / 2 - dir * (model.size + hButtonGap) / 2
                                , -model.size / 2
                                )

                            else
                                ( -model.size / 2
                                , -model.size / 2 + dir * (model.size + vButtonGap) / 2
                                )
                    ]
                )
                []

        text offset value =
            Svg.text_
                [ SA.fontFamily "mono"
                , SA.textAnchor "middle"
                , SA.alignmentBaseline "middle"
                , SA.fontSize <| String.fromFloat fontSize
                , SA.fill <| colorToCssRgba model.color
                , SA.transform <| translate offset
                ]
                [ Svg.text value ]

        buttonText isPlus =
            let
                dir =
                    if isPlus then
                        1

                    else
                        -1

                offset =
                    if model.isHorizontal then
                        ( dir * (hButtonGap + model.size) / 2, 0 )

                    else
                        ( 0, -dir * (model.size + vButtonGap) / 2 )

                value =
                    if isPlus then
                        "+"

                    else
                        "-"
            in
            text offset value

        -- TODO: Now realizing the abstraction I've been searching for
        -- *facepalm* the buttons themselves should be single Svg.g components
        -- that can then be moved around.
        plusButtonSvgs =
            case model.onPlus of
                Just _ ->
                    [ rect True
                        Nothing
                        model.backgroundColor
                        True
                    , buttonText True

                    -- Hit box
                    , rect False
                        model.onPlus
                        invisible
                        True
                    ]

                Nothing ->
                    []

        minusButtonSvgs =
            case model.onMinus of
                Just _ ->
                    [ rect True
                        Nothing
                        model.backgroundColor
                        False
                    , buttonText False

                    -- Hit box
                    , rect False
                        model.onMinus
                        invisible
                        False
                    ]

                Nothing ->
                    []
    in
    Svg.g
        [ SA.transform <|
            translate
                ( if model.isHorizontal then
                    0

                  else
                    model.offset
                , if model.isHorizontal then
                    model.offset

                  else
                    0
                )
        ]
        (plusButtonSvgs
            ++ [ text ( 0, 0 ) (String.fromInt model.count) ]
            ++ minusButtonSvgs
        )


type alias Model a =
    { radius : Float
    , size : Float
    , color : Color
    , offset : ( Float, Float )
    , backgroundColor : Color
    , counts : Arc.Arc Int
    , onPlus : Arc.Arc (Maybe a)
    , onMinus : Arc.Arc (Maybe a)
    }


view : Model a -> Svg a
view model =
    Svg.g
        [ SA.transform <| translate model.offset ]
        (Arc.foldWithAnArc
            (\arc ( count, onPlus, onMinus ) list ->
                single
                    { count = count
                    , size = model.size
                    , isHorizontal = arc == Forward || arc == Aft
                    , offset =
                        (model.radius - model.size / 2)
                            * (if arc == Aft || arc == Starboard then
                                1

                               else
                                -1
                              )
                    , color = model.color
                    , backgroundColor = model.backgroundColor
                    , onPlus = onPlus
                    , onMinus = onMinus
                    }
                    :: list
            )
            []
            (Arc.liftA3 (\a b c -> ( a, b, c )) model.counts model.onPlus model.onMinus)
        )
