module Main exposing (Model, Msg(..), initialModel, update, view)

import Arc
import Browser exposing (element)
import BuildClient exposing (CreateStarshipBuildError, GetStarshipBuildError, HttpClientError, createStarshipBuild, createStarshipBuildErrorToString, getStarshipBuild, getStarshipBuildErrorToString, httpClientErrorToString)
import CognitoClient
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (disabled, value)
import Html.Events exposing (onClick, onInput)
import Login
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import ShipAssets
import Starship exposing (Starship)
import StarshipEditor
import Togglable
import Weapon


initialModel : Model
initialModel =
    { putResult = Nothing
    , getResult = Nothing
    , idToken = ""
    , hostName = ""
    , userId = ""
    }


type alias Model =
    { putResult : Maybe (Result (HttpClientError CreateStarshipBuildError) String)
    , getResult : Maybe (Result (HttpClientError GetStarshipBuildError) ( String, Starship ))
    , idToken : String
    , hostName : String
    , userId : String
    }


type Msg
    = CreateStarshipBuildResult (Result (HttpClientError CreateStarshipBuildError) String)
    | GetStarshipBuildResult (Result (HttpClientError GetStarshipBuildError) ( String, Starship ))
    | SendPutRequest
    | SendGetRequest
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
update msg ({ getResult, putResult, idToken, hostName, userId } as s) =
    case msg of
        SendPutRequest ->
            ( s, Cmd.map CreateStarshipBuildResult (createStarshipBuild hostName userId idToken ship) )

        SendGetRequest ->
            ( s, Cmd.map GetStarshipBuildResult (getStarshipBuild hostName userId idToken "THE NAME") )

        CreateStarshipBuildResult r ->
            ( { s | putResult = Just r }, Cmd.none )

        GetStarshipBuildResult r ->
            ( { s | getResult = Just r }, Cmd.none )

        SetIdToken t ->
            ( { s | idToken = t }, Cmd.none )

        SetHostName hn ->
            ( { s | hostName = hn }, Cmd.none )

        SetUserId ui ->
            ( { s | userId = ui }, Cmd.none )

        Back ->
            ( { s | putResult = Nothing, getResult = Nothing }, Cmd.none )


view : Model -> Html Msg
view ({ getResult, putResult, idToken, hostName, userId } as s) =
    div
        []
        (case ( putResult, getResult ) of
            ( Just (Ok eTag), _ ) ->
                [ text ("DONE: " ++ eTag)
                , button [ onClick Back ] [ text "BACK" ]
                ]

            ( Just (Err e), _ ) ->
                [ text ("ERROR: " ++ httpClientErrorToString createStarshipBuildErrorToString e)
                , button [ onClick Back ] [ text "BACK" ]
                ]

            ( _, Just (Ok ( eTag, starship )) ) ->
                [ text ("eTag: " ++ eTag)

                -- TODO: No point in edits at the moment, they can't be saved
                , Html.map (always Back) <| StarshipEditor.view starship
                , button [ onClick Back ] [ text "BACK" ]
                ]

            ( _, Just (Err e) ) ->
                [ text ("ERROR: " ++ httpClientErrorToString getStarshipBuildErrorToString e)
                , button [ onClick Back ] [ text "BACK" ]
                ]

            ( Nothing, Nothing ) ->
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
                , button [ onClick SendPutRequest ] [ text "PUT" ]
                , button [ onClick SendGetRequest ] [ text "GET" ]
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
