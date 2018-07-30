module SvgUtils exposing (Segment(..), Strategy(..), Point, Path, d)

--TODO: This is lifted from geo and should become a package!!!

import Svg exposing (..)
import Svg.Attributes exposing (..)
import String


type Strategy
    = Absolute
    | Relative


type alias Point =
    ( Float, Float )


type Segment
    = MoveTo Point
    | Horizontal Float
    | Vertical Float
    | Linear Point
    | Quadratic Point Point
    | Cubic Point Point Point
    | SymQuadratic Point
    | SymCubic Point Point
    | Arc Point Float Bool Bool Point


type alias Path =
    ( Bool, List ( Strategy, Segment ) )


stratStr : String -> Strategy -> String
stratStr char strat =
    case strat of
        Absolute ->
            String.toUpper char

        Relative ->
            String.toLower char


pointStr : Point -> String
pointStr ( x, y ) =
    toString x ++ "," ++ toString y


flagStr : Bool -> String
flagStr b =
    if b then
        "1"
    else
        "0"


segmentChar : Segment -> String
segmentChar segment =
    case segment of
        MoveTo _ ->
            "m"

        Horizontal _ ->
            "h"

        Vertical _ ->
            "v"

        Linear _ ->
            "l"

        Quadratic _ _ ->
            "q"

        Cubic _ _ _ ->
            "c"

        SymQuadratic _ ->
            "t"

        SymCubic _ _ ->
            "s"

        Arc _ _ _ _ _ ->
            "a"


segmentStr : Segment -> String
segmentStr segment =
    case segment of
        MoveTo p ->
            pointStr p

        Horizontal x ->
            toString x

        Vertical y ->
            toString y

        Linear p ->
            pointStr p

        Quadratic cp p ->
            pointStr cp ++ "," ++ pointStr p

        Cubic c1p c2p p ->
            pointStr c1p ++ "," ++ pointStr c2p ++ "," ++ pointStr p

        SymQuadratic p ->
            pointStr p

        SymCubic cp p ->
            pointStr cp ++ "," ++ pointStr p

        Arc origin xAxisRotate largeArcFlag sweepFlag p ->
            pointStr origin ++ " " ++ toString xAxisRotate ++ " " ++ flagStr largeArcFlag ++ " " ++ flagStr sweepFlag ++ " " ++ pointStr p


stratSegStr : ( Strategy, Segment ) -> String
stratSegStr ( strategy, segment ) =
    stratStr (segmentChar segment) strategy ++ segmentStr segment


pathStr : Path -> String
pathStr ( closed, p ) =
    List.foldr (stratSegStr >> (++)) "" p
        ++ if closed then
            "z"
           else
            ""


d : Path -> Svg.Attribute a
d =
    pathStr >> Svg.Attributes.d
