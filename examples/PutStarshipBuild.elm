module Main exposing (Model, Msg(..), initialModel, update, view)

import Arc
import Browser exposing (element)
import BuildClient exposing (CreateStarshipBuildError, GetStarshipBuildError, HttpClientError, UpdateStarshipBuildError, createStarshipBuild, createStarshipBuildErrorToString, getStarshipBuild, getStarshipBuildErrorToString, httpClientErrorToString, updateStarshipBuild, updateStarshipBuildErrorToString)
import CognitoClient
import Dict
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (disabled, value)
import Html.Events exposing (onClick, onInput)
import InputConfigured as IC
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
    , updateResult = Nothing
    , idToken = ""
    , hostName = ""
    , userId = ""
    , shipName = ""
    }


type alias Model =
    { putResult : Maybe (Result (HttpClientError CreateStarshipBuildError) String)
    , getResult : Maybe (Result (HttpClientError GetStarshipBuildError) ( String, Starship ))

    -- Nothing -> Not Updating
    -- Just Nothing -> Updating
    -- Just (Just error) -> Update failed
    , updateResult : Maybe (Maybe (HttpClientError UpdateStarshipBuildError))
    , idToken : String
    , hostName : String
    , userId : String
    , shipName : String
    }


type Msg
    = CreateStarshipBuildResult (Result (HttpClientError CreateStarshipBuildError) String)
    | GetStarshipBuildResult (Result (HttpClientError GetStarshipBuildError) ( String, Starship ))
    | UpdateStarshipBuildResult (Result (HttpClientError UpdateStarshipBuildError) String)
    | SendPutRequest
    | SendGetRequest
    | SendUpdateRequest
    | SetShipName String
    | StarshipUpdate StarshipEditor.Msg
    | Back


ship name =
    { name = name
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


update : { a | idToken : String, hostName : String, userId : String } -> Msg -> Model -> ( Model, Cmd Msg )
update { idToken, hostName, userId } msg ({ getResult, putResult, updateResult, shipName } as s) =
    case msg of
        SendPutRequest ->
            ( s, Cmd.map CreateStarshipBuildResult (createStarshipBuild hostName userId idToken (ship shipName)) )

        SendGetRequest ->
            ( s, Cmd.map GetStarshipBuildResult (getStarshipBuild hostName userId idToken shipName) )

        SendUpdateRequest ->
            case getResult of
                Just (Ok ( eTag, starship )) ->
                    ( { s | updateResult = Just Nothing }, Cmd.map UpdateStarshipBuildResult (updateStarshipBuild hostName userId idToken eTag starship) )

                _ ->
                    ( s, Cmd.none )

        CreateStarshipBuildResult r ->
            ( { s | putResult = Just r }, Cmd.none )

        GetStarshipBuildResult r ->
            ( { s | getResult = Just r }, Cmd.none )

        UpdateStarshipBuildResult r ->
            case r of
                Err x ->
                    ( { s | updateResult = Just (Just x) }, Cmd.none )

                Ok eTag ->
                    case getResult of
                        Just (Ok ( _, starship )) ->
                            ( { s
                                | updateResult = Nothing
                                , getResult = Just (Ok ( eTag, starship ))
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( s, Cmd.none )

        SetShipName sn ->
            ( { s | shipName = sn }, Cmd.none )

        StarshipUpdate sMsg ->
            case getResult of
                Just (Ok ( eTag, sModel )) ->
                    ( { s | getResult = Just (Ok ( eTag, StarshipEditor.update sMsg sModel )) }, Cmd.none )

                _ ->
                    ( s, Cmd.none )

        Back ->
            case updateResult of
                Just (Just _) ->
                    ( { s | updateResult = Nothing }, Cmd.none )

                _ ->
                    ( { s | putResult = Nothing, getResult = Nothing, updateResult = Nothing }, Cmd.none )


view : Model -> Html Msg
view ({ getResult, putResult, updateResult, shipName } as s) =
    div
        []
        (case ( putResult, getResult, updateResult ) of
            ( _, _, Just (Just e) ) ->
                [ text ("ERROR: " ++ httpClientErrorToString updateStarshipBuildErrorToString e)
                , button [ onClick Back ] [ text "BACK" ]
                ]

            ( _, _, Just Nothing ) ->
                [ text "UPDATING" ]

            ( Just (Ok eTag), _, _ ) ->
                [ text ("DONE: " ++ eTag)
                , button [ onClick Back ] [ text "BACK" ]
                ]

            ( Just (Err e), _, _ ) ->
                [ text ("ERROR: " ++ httpClientErrorToString createStarshipBuildErrorToString e)
                , button [ onClick Back ] [ text "BACK" ]
                ]

            ( _, Just (Ok ( eTag, starship )), _ ) ->
                [ text ("eTag: " ++ eTag)
                , Html.map StarshipUpdate <| StarshipEditor.view starship
                , button [ onClick SendUpdateRequest ] [ text "UPDATE" ]
                , button [ onClick Back ] [ text "BACK" ]
                ]

            ( _, Just (Err e), _ ) ->
                [ text ("ERROR: " ++ httpClientErrorToString getStarshipBuildErrorToString e)
                , button [ onClick Back ] [ text "BACK" ]
                ]

            ( Nothing, Nothing, Nothing ) ->
                [ div []
                    [ label [] [ text "Starship Build Name:" ]
                    , input [ onInput SetShipName, value shipName ] []
                    ]
                , button [ onClick SendPutRequest ] [ text "PUT" ]
                , button [ onClick SendGetRequest ] [ text "GET" ]
                ]
        )


main : Program () (IC.Model Model) (IC.Msg Msg)
main =
    element
        { init =
            IC.initialModel
                (Dict.fromList
                    [ ( "hostName", "" )
                    , ( "userId", "" )
                    , ( "idToken", "" )
                    ]
                )
                (\_ -> ( initialModel, Cmd.none ))
        , update =
            IC.update
                (\c ->
                    { hostName = Maybe.withDefault "Not Configured" <| Dict.get "hostName" c
                    , idToken = Maybe.withDefault "Not Configured" <| Dict.get "idToken" c
                    , userId = Maybe.withDefault "Not Configured" <| Dict.get "userId" c
                    }
                )
                update
        , view =
            IC.view always (\_ -> view)
        , subscriptions =
            \_ -> Sub.none
        }
