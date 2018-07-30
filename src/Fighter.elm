module Fighter exposing (..)

import Html exposing (Html, div, button, text, beginnerProgram)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)


type alias Model =
    { size : Float
    , color : Color
    }


fighter : Model -> Svg a
fighter model =
    -- TODO: the figher doesn't quite fill its bounding box
    let
        scale =
            "scale(" ++ toString (model.size / 12800 / sqrt 2) ++ ")"

        translate =
            "translate(" ++ toString (-12800 * (1 - sqrt 2) / 2) ++ "," ++ toString (-12800 * (1 - sqrt 2) / 2) ++ ")"

        rotate =
            "rotate(-137.5," ++ toString 6400 ++ "," ++ toString 6400 ++ ")"
    in
        Svg.g
            [ SA.transform (scale ++ translate ++ rotate)
            , SA.fill (colorToCssRgb model.color)
            ]
            [ Svg.path [ SA.d "M12190 12766 c-396 -139 -1244 -536 -1439 -674 -407 -288 -1499 -1264 -1942 -1737 -266 -284 -356 -369 -525 -490 l-104 -75 -332 294 -331 294 -356 -326 c-196 -179 -369 -338 -386 -353 l-30 -28 -820 -105 c-1154 -148 -1344 -170 -1855 -215 -727 -65 -1222 -92 -2040 -111 -964 -23 -1524 -66 -1595 -122 -159 -125 -494 -1040 -426 -1165 12 -24 53 -35 461 -118 497 -102 1491 -287 2635 -490 285 -50 578 -102 925 -164 124 -22 392 -70 595 -106 204 -36 373 -67 377 -68 3 -1 -110 -135 -252 -298 l-258 -296 -119 -6 c-65 -3 -905 -36 -1867 -73 l-1748 -69 -122 -135 c-225 -250 -276 -317 -276 -360 1 -46 -19 -31 385 -285 463 -292 1521 -986 1810 -1188 139 -97 163 -122 182 -186 20 -72 13 -144 -30 -293 l-34 -117 295 -278 c162 -153 315 -296 340 -318 l45 -41 -48 -84 c-83 -146 -183 -307 -332 -539 -140 -216 -233 -385 -233 -423 0 -59 114 14 425 271 286 236 548 441 564 441 3 0 44 -35 91 -77 256 -229 500 -439 557 -479 97 -70 115 -70 281 10 193 92 227 90 328 -16 116 -121 859 -1007 1463 -1743 144 -176 273 -326 287 -333 19 -10 30 -10 56 1 35 15 142 121 283 282 49 55 102 114 117 131 l29 31 -81 999 c-44 550 -103 1271 -130 1604 -27 333 -59 721 -70 863 l-21 257 28 34 c69 82 488 546 493 546 5 0 84 -254 482 -1560 555 -1819 946 -3052 1024 -3229 l24 -54 51 7 c149 20 688 253 926 400 166 102 187 149 186 411 -1 194 -14 356 -83 1035 -96 949 -114 1162 -145 1780 -32 642 -38 1045 -31 2162 l7 1098 305 426 c169 234 302 429 298 433 -26 23 -640 565 -648 572 -10 9 87 180 185 324 71 105 236 315 359 456 324 375 1190 1624 1415 2043 63 117 135 302 241 622 142 429 283 933 284 1012 0 29 -15 28 -130 -12z" ] []
            ]


asHtml : Model -> Svg a
asHtml model =
    let
        sizeStr =
            toString model.size
    in
        div []
            [ Svg.svg
                [ SA.height sizeStr
                , SA.width sizeStr
                , SA.viewBox ("0 0 " ++ sizeStr ++ " " ++ sizeStr)
                ]
                [ fighter model ]
            ]



--TODO: Move to examples


main : Program Never Model a
main =
    beginnerProgram
        { model = { size = 400, color = Color.green }
        , update = always identity
        , view = asHtml
        }
