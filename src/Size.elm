module Size exposing (Size(..), toString, topSpeed)


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


toString : Size -> String
toString size =
    case size of
        Tiny ->
            "Tiny"

        Small ->
            "Small"

        Medium ->
            "Medium"

        Large ->
            "Large"

        Huge ->
            "Huge"

        Gargantuan ->
            "Gargantuan"

        Colossal ->
            "Colossal"
