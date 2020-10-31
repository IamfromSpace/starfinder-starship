module Size exposing (Size(..), getAcModifier, getTlModifier, toString, topSpeed)


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


acAndTlBonus : Size -> Int
acAndTlBonus size =
    case size of
        Tiny ->
            2

        Small ->
            1

        Medium ->
            0

        Large ->
            -1

        Huge ->
            -2

        Gargantuan ->
            -4

        Colossal ->
            -8


getAcModifier : Size -> Int
getAcModifier =
    acAndTlBonus


getTlModifier : Size -> Int
getTlModifier =
    acAndTlBonus


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
