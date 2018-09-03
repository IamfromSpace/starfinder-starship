module Switch exposing (Switch(..), toString, toggle)


type Switch
    = On
    | Off


toString : Switch -> String
toString switch =
    case switch of
        On ->
            "On"

        Off ->
            "Off"


toggle : Switch -> Switch
toggle switch =
    case switch of
        On ->
            Off

        Off ->
            On
