module Switch exposing (..)


type Switch
    = On
    | Off


toggle : Switch -> Switch
toggle switch =
    case switch of
        On ->
            Off

        Off ->
            On
