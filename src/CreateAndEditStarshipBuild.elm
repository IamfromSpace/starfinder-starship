module CreateAndEditStarshipBuild exposing (Model, Msg(..), initialModel, update, view)

import Arc
import BuildClient exposing (CreateStarshipBuildError, GetStarshipBuildError, GetStarshipBuildsError(..), HttpClientError, Link, StarshipBuildLink, UpdateStarshipBuildError, createStarshipBuild, createStarshipBuildErrorToString, getStarshipBuild, getStarshipBuildErrorToString, getStarshipBuilds, getStarshipBuildsErrorToString, httpClientErrorToString, updateStarshipBuild, updateStarshipBuildErrorToString)
import Dict
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (disabled, value)
import Html.Events exposing (onClick, onInput)
import InputConfigured as IC
import KeyedSet as KS
import Platform.Cmd exposing (Cmd)
import ShipAssets
import Starship exposing (Starship)
import StarshipEditor
import Togglable
import Weapon


initialModel : Model
initialModel =
    { starshipBuild = Nothing
    , error = Nothing
    , isFetching = False
    , shipName = ""
    , ships = Nothing
    }


type alias Model =
    -- TODO: Store the previous Starship Build state so we can detect if it's
    -- actually changed
    { starshipBuild : Maybe ( Maybe ( String, Link ), Starship )
    , error : Maybe String
    , isFetching : Bool
    , shipName : String
    , ships : Maybe (List StarshipBuildLink)
    }


type Msg
    = CreateStarshipBuildResult (Result (HttpClientError CreateStarshipBuildError) ( String, Link ))
    | GetStarshipBuildResult Link (Result (HttpClientError GetStarshipBuildError) ( String, Starship ))
    | GetStarshipBuildsResult (Result (HttpClientError GetStarshipBuildsError) (List StarshipBuildLink))
    | UpdateStarshipBuildResult (Result (HttpClientError UpdateStarshipBuildError) String)
    | CreateShip
    | SaveShip
    | GetShip Link
    | GetShips
    | SetShipName String
    | StarshipUpdate StarshipEditor.Msg
    | Back



-- TODO: The config shouldn't be an idToken and hostname, this should be our client


update : { a | idToken : String, hostName : String } -> Msg -> Model -> ( Model, Cmd Msg )
update { idToken, hostName } msg ({ starshipBuild, error, isFetching, shipName } as s) =
    case msg of
        GetShip link ->
            ( { s | isFetching = True }
            , Cmd.map
                (GetStarshipBuildResult link)
                (getStarshipBuild idToken link)
            )

        GetShips ->
            ( { s | isFetching = True }
            , Cmd.map
                GetStarshipBuildsResult
                (getStarshipBuilds hostName idToken)
            )

        CreateShip ->
            ( { s | starshipBuild = Just ( Nothing, StarshipEditor.init ) }, Cmd.none )

        SaveShip ->
            case starshipBuild of
                Just ( Just ( eTag, link ), starship ) ->
                    ( { s | isFetching = True }
                    , Cmd.map
                        UpdateStarshipBuildResult
                        (updateStarshipBuild idToken link eTag starship)
                    )

                Just ( Nothing, starship ) ->
                    ( { s | isFetching = True }
                    , Cmd.map
                        CreateStarshipBuildResult
                        (createStarshipBuild hostName idToken starship)
                    )

                _ ->
                    ( s, Cmd.none )

        CreateStarshipBuildResult r ->
            if isFetching then
                case r of
                    Err x ->
                        ( { s
                            | error =
                                Just (httpClientErrorToString createStarshipBuildErrorToString x)
                            , isFetching = False
                          }
                        , Cmd.none
                        )

                    Ok ( eTag, link ) ->
                        case starshipBuild of
                            Just ( _, sb ) ->
                                ( { s
                                    | isFetching = False
                                    , starshipBuild = Just ( Just ( eTag, link ), sb )
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                ( s, Cmd.none )

            else
                ( s, Cmd.none )

        GetStarshipBuildsResult r ->
            if isFetching then
                case r of
                    Err x ->
                        ( { s
                            | error =
                                Just (httpClientErrorToString getStarshipBuildsErrorToString x)
                            , isFetching = False
                          }
                        , Cmd.none
                        )

                    Ok ships ->
                        ( { s
                            | isFetching = False
                            , ships = Just ships
                          }
                        , Cmd.none
                        )

            else
                ( s, Cmd.none )

        GetStarshipBuildResult link r ->
            if isFetching then
                case r of
                    Err x ->
                        ( { s
                            | error =
                                Just (httpClientErrorToString getStarshipBuildErrorToString x)
                            , isFetching = False
                          }
                        , Cmd.none
                        )

                    Ok ( eTag, sb ) ->
                        ( { s
                            | isFetching = False
                            , starshipBuild = Just ( Just ( eTag, link ), sb )
                          }
                        , Cmd.none
                        )

            else
                ( s, Cmd.none )

        UpdateStarshipBuildResult r ->
            if isFetching then
                case r of
                    Err x ->
                        ( { s
                            | error =
                                Just (httpClientErrorToString updateStarshipBuildErrorToString x)
                            , isFetching = False
                          }
                        , Cmd.none
                        )

                    Ok eTag ->
                        case starshipBuild of
                            Just ( Just ( _, link ), sb ) ->
                                ( { s
                                    | isFetching = False
                                    , starshipBuild = Just ( Just ( eTag, link ), sb )
                                  }
                                , Cmd.none
                                )

                            -- This should be impossible, right?
                            Just ( Nothing, _ ) ->
                                ( s, Cmd.none )

                            Nothing ->
                                ( s, Cmd.none )

            else
                ( s, Cmd.none )

        SetShipName sn ->
            ( { s | shipName = sn }, Cmd.none )

        StarshipUpdate sMsg ->
            case starshipBuild of
                Just ( eTag, sModel ) ->
                    ( { s | starshipBuild = Just ( eTag, StarshipEditor.update sMsg sModel ) }, Cmd.none )

                _ ->
                    ( s, Cmd.none )

        Back ->
            case error of
                Just _ ->
                    ( { s | error = Nothing }, Cmd.none )

                Nothing ->
                    -- TODO: This could lose saved progress!
                    ( { s | starshipBuild = Nothing }, Cmd.none )


view : Model -> Html Msg
view { starshipBuild, error, isFetching, shipName, ships } =
    div
        []
        (case ( starshipBuild, error, isFetching ) of
            ( _, _, True ) ->
                [ text "..." ]

            ( _, Just e, _ ) ->
                [ text ("ERROR: " ++ e)
                , button [ onClick Back ] [ text "BACK" ]
                ]

            ( Just ( _, starship ), _, _ ) ->
                [ Html.map StarshipUpdate <| StarshipEditor.view starship
                , button [ onClick SaveShip ] [ text "SAVE" ]
                , button [ onClick Back ] [ text "BACK" ]
                ]

            ( Nothing, _, _ ) ->
                [ button [ onClick CreateShip ] [ text "CREATE NEW" ]
                , button [ onClick GetShips ] [ text "GET ALL SHIP NAMES" ]

                -- TODO: This could obviously be much better
                , div []
                    (case ships of
                        Just s ->
                            div [] [ text "Current Ships:" ]
                                :: List.map
                                    (\ship ->
                                        div []
                                            [ button
                                                [ onClick (GetShip ship.link)
                                                ]
                                                [ text ship.name ]
                                            ]
                                    )
                                    s

                        Nothing ->
                            [ div [] [ text "Ships Not Fetched" ] ]
                    )
                ]
        )
