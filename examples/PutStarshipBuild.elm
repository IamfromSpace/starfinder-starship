module Main exposing (main)

import Arc
import Browser exposing (element)
import BuildClient as BC
import CreateAndEditStarshipBuild exposing (Config, Model, Msg, initialModel, update, view)
import Dict
import Html.Attributes exposing (disabled, value)
import Html.Events exposing (onClick, onInput)
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import StarshipEditor


config : Config {}
config =
    { createStarshipBuild = BC.mockCreateStarshipBuild
    , getStarshipBuild = BC.makeMockGetStarshipBuild StarshipEditor.init
    , updateStarshipBuild = BC.makeMockUpdateStarshipBuild StarshipEditor.init
    , getStarshipBuilds = BC.mockGetStarshipBuilds
    }


main : Program () Model Msg
main =
    element
        { init =
            \_ -> ( initialModel, Cmd.none )
        , update =
            update config
        , view =
            view
        , subscriptions =
            \_ -> Sub.none
        }
