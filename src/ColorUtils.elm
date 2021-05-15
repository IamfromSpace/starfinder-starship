module ColorUtils exposing (colorTransition)

import Color exposing (Color, green, grey, red, yellow)
import Color.Manipulate exposing (weightedMix)


colorTransition : Float -> Color
colorTransition x =
    if x > 1 then
        weightedMix Color.blue green ((x - 1) / 1.8)

    else if x > 0.5 then
        weightedMix green yellow (2 * (x - 0.5))

    else if x > 0 then
        weightedMix yellow red (2 * x)

    else
        grey
