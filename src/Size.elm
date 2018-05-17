module Size exposing (..)


type Size
    = Tiny
    | Small
    | Medium
    | Large
    | Huge
    | Gargantuan
    | Colossal


topSpeed : Size -> Int
topSpeed size =
    case size of
        Tiny ->
            14

        Small ->
            12

        Medium ->
            12

        Large ->
            10

        Huge ->
            10

        _ ->
            8
