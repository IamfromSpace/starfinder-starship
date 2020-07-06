module Main exposing (Model, Msg(..), initialModel, update, view)

import Arc
import Browser exposing (element)
import BuildClient exposing (CreateStarshipBuildError, createStarshipBuild, createStarshipBuildErrorToString)
import CognitoClient
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (disabled, value)
import Html.Events exposing (onClick, onInput)
import Login
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import ShipAssets
import Starship
import Togglable
import Weapon


initialModel : Model
initialModel =
    { result = Nothing
    , idToken = ""
    , hostName = ""
    , userId = ""
    }


type alias Model =
    { result : Maybe (Result CreateStarshipBuildError String)
    , idToken : String
    , hostName : String
    , userId : String
    }


type Msg
    = CreateStarshipBuildResult (Result CreateStarshipBuildError String)
    | SendRequest
    | SetIdToken String
    | SetHostName String
    | SetUserId String
    | Back


ship =
    { name = "THE NAME"
    , frame = ShipAssets.mediumTransport
    , powerCoreUnits = 200
    , thrusters = Togglable.pure 8
    , armor = Nothing
    , computer = Togglable.pure { bonus = 0, nodes = 0 }
    , crewQuarters = Starship.Common
    , defensiveCountermeasures = Nothing
    , driftEngine = Nothing
    , expansionBays = []
    , sensors = { bonus = 0, range = Weapon.Short }
    , arcWeapons = Arc.pure []
    , turretWeapons = []
    , shields = Togglable.pure ShipAssets.lightShields60
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ result, idToken, hostName, userId } as s) =
    case msg of
        SendRequest ->
            ( s, Cmd.map CreateStarshipBuildResult (createStarshipBuild hostName userId idToken ship) )

        CreateStarshipBuildResult r ->
            ( { s | result = Just r }, Cmd.none )

        SetIdToken t ->
            ( { s | idToken = t }, Cmd.none )

        SetHostName hn ->
            ( { s | hostName = hn }, Cmd.none )

        SetUserId ui ->
            ( { s | userId = ui }, Cmd.none )

        Back ->
            ( { s | result = Nothing }, Cmd.none )


view : Model -> Html Msg
view ({ result, idToken, hostName, userId } as s) =
    div
        []
        (case result of
            Just (Ok eTag) ->
                [ text ("DONE: " ++ eTag)
                , button [ onClick Back ] [ text "BACK" ]
                ]

            Just (Err e) ->
                [ text ("ERROR: " ++ createStarshipBuildErrorToString e)
                , button [ onClick Back ] [ text "BACK" ]
                ]

            Nothing ->
                [ div []
                    [ label [] [ text "Host Name:" ]
                    , input [ onInput SetHostName, value hostName ] []
                    ]
                , div []
                    [ label [] [ text "User Id:" ]
                    , input [ onInput SetUserId, value userId ] []
                    ]
                , div []
                    [ label [] [ text "Id Token:" ]
                    , input [ onInput SetIdToken, value idToken ] []
                    ]
                , button [ onClick SendRequest ] [ text "PUT" ]
                ]
        )


main : Program () Model Msg
main =
    element
        { init =
            \_ -> ( initialModel, Cmd.none )
        , update = update
        , view =
            view
        , subscriptions = \_ -> Sub.none
        }
