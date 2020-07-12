module Main exposing (main)

import Arc
import Browser exposing (element)
import CreateAndEditStarshipBuild exposing (Model, Msg, initialModel, update, view)
import Dict
import Html.Attributes exposing (disabled, value)
import Html.Events exposing (onClick, onInput)
import InputConfigured as IC
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)


main : Program () (IC.Model Model) (IC.Msg Msg)
main =
    element
        { init =
            IC.initialModel
                (Dict.fromList
                    [ ( "hostName", "" )
                    , ( "idToken", "" )
                    ]
                )
                (\_ -> ( initialModel, Cmd.none ))
        , update =
            IC.update
                (\c ->
                    { hostName = Maybe.withDefault "Not Configured" <| Dict.get "hostName" c
                    , idToken = Maybe.withDefault "Not Configured" <| Dict.get "idToken" c
                    }
                )
                update
        , view =
            IC.view always (\_ -> view)
        , subscriptions =
            \_ -> Sub.none
        }
