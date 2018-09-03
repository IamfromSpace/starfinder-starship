module ShieldArc exposing (Model, asHtml, getHeight, getIsConcaveUp, getIsWide, main, mapBoth, view)

import Arc exposing (AnArc(..))
import Browser exposing (sandbox)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)
import Html exposing (Html, button, div, text)
import Svg exposing (Svg)
import Svg.Attributes as SA
import SvgUtils as SU exposing (Segment(..), Strategy(..))



-- TODO: This whole module is very focused on getting a single arc of any
-- orientation to fit at origin (0,0), so it can then be manipulated later
-- in a HTML-ish way of stacking divs horizontally and vertically.
-- however, being SVG, this kind of misses the point.  Instead, it should
-- make a component that takes another component and builds the four shield
-- arcs around it.
-- The math is much simpler because we can do a single offset, and then
-- rotate around the center, and then we don't need any of our 'asHtml' functions


mapBoth : (a -> b) -> ( a, a ) -> ( b, b )
mapBoth fn ( x, y ) =
    ( fn x, fn y )


type alias Model =
    { size : Float
    , color : Color
    , arc : AnArc
    }


getIsWide : AnArc -> Bool
getIsWide a =
    case a of
        Forward ->
            True

        Aft ->
            True

        _ ->
            False



-- get the direction that the shield is pointing using some
-- good ol' calculus definitions.


getIsConcaveUp : AnArc -> Bool
getIsConcaveUp a =
    case a of
        Forward ->
            True

        Starboard ->
            -- yes?
            True

        _ ->
            False


getHeight : Float -> Float
getHeight size =
    (sqrt 2 - 1) * size / 2


view : Model -> Svg a
view model =
    let
        outerRadius =
            model.size * sqrt 2 / 2

        innerRadius =
            model.size

        isConcaveUp =
            getIsConcaveUp model.arc

        origin =
            case model.arc of
                Forward ->
                    ( 0, getHeight model.size )

                Port ->
                    ( getHeight model.size, 0 )

                _ ->
                    ( 0, 0 )

        delta =
            if getIsWide model.arc then
                ( model.size, 0 )

            else
                ( 0, model.size )
    in
    Svg.path
        [ SA.fill (colorToCssRgb model.color)
        , SU.d
            ( True
            , [ ( Absolute, MoveTo origin )
              , ( Relative, Arc ( outerRadius, outerRadius ) 0 False isConcaveUp delta )
              , ( Relative, Arc ( innerRadius, innerRadius ) 0 False (not isConcaveUp) (mapBoth ((*) -1) delta) )
              ]
            )
        ]
        []


asHtml : Model -> Svg a
asHtml model =
    let
        shortStr =
            String.fromFloat (getHeight model.size)

        longStr =
            String.fromFloat model.size

        isWide =
            getIsWide model.arc

        heightStr =
            if isWide then
                shortStr

            else
                longStr

        widthStr =
            if isWide then
                longStr

            else
                shortStr
    in
    div []
        [ Svg.svg
            [ SA.height heightStr
            , SA.width widthStr
            , SA.viewBox ("0 0 " ++ widthStr ++ " " ++ heightStr)
            ]
            [ view model ]
        ]



--TODO: Move to examples


main : Program () Model a
main =
    sandbox
        { init = { arc = Forward, size = 400, color = Color.green }
        , update = always identity
        , view = asHtml
        }
