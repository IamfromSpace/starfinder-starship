module CreateAndEditStarshipBuild exposing (Config, Model, Msg(..), initialModel, update, view)

import Arc
import BuildClient exposing (CreateStarshipBuild, CreateStarshipBuildError, GetStarshipBuild, GetStarshipBuildError, GetStarshipBuilds, GetStarshipBuildsError(..), HttpClientError, Link, StarshipBuildLink, UpdateStarshipBuild, UpdateStarshipBuildError, createStarshipBuild, createStarshipBuildErrorToString, getStarshipBuild, getStarshipBuildErrorToString, getStarshipBuilds, getStarshipBuildsErrorToString, httpClientErrorToString, updateStarshipBuild, updateStarshipBuildErrorToString)
import CrewEditor exposing (Crew, emptyCrew)
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
import StatusEditor
import Togglable
import Weapon


initialModel : Model
initialModel =
    { starshipBuild = Nothing
    , flightStatus = Nothing
    , error = Nothing
    , isFetching = False
    , shipName = ""
    , ships = Nothing
    }


type FlightStatus
    = SelectingCrew Crew
    | Flying StatusEditor.Model


type alias Model =
    -- TODO: Store the previous Starship Build state so we can detect if it's
    -- actually changed
    { starshipBuild : Maybe ( Maybe ( String, Link ), Starship )
    , flightStatus : Maybe FlightStatus
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
    | FlyStarshipGetResult (Result (HttpClientError GetStarshipBuildError) ( String, Starship ))
    | CreateShip
    | SaveShip
    | GetShip Link
    | GetShips
    | FlyShip Link
    | SetShipName String
    | StarshipUpdate StarshipEditor.Msg
    | StatusUpdate StatusEditor.Msg
    | CrewUpdate Crew
    | AcceptCrew
    | Back


type alias Config a =
    { a
        | getStarshipBuild : GetStarshipBuild
        , getStarshipBuilds : GetStarshipBuilds
        , createStarshipBuild : CreateStarshipBuild
        , updateStarshipBuild : UpdateStarshipBuild
    }


update :
    Config a
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
update { getStarshipBuild, getStarshipBuilds, createStarshipBuild, updateStarshipBuild } msg ({ starshipBuild, error, isFetching, shipName, flightStatus } as s) =
    case msg of
        GetShip link ->
            ( { s | isFetching = True }
            , Cmd.map
                (GetStarshipBuildResult link)
                (getStarshipBuild link)
            )

        GetShips ->
            ( { s | isFetching = True }
            , Cmd.map
                GetStarshipBuildsResult
                getStarshipBuilds
            )

        CreateShip ->
            ( { s | starshipBuild = Just ( Nothing, StarshipEditor.init ) }, Cmd.none )

        FlyShip link ->
            ( { s | isFetching = True }
            , Cmd.map
                FlyStarshipGetResult
                (getStarshipBuild link)
            )

        SaveShip ->
            case starshipBuild of
                Just ( Just ( eTag, link ), starship ) ->
                    ( { s | isFetching = True }
                    , Cmd.map
                        UpdateStarshipBuildResult
                        (updateStarshipBuild link eTag starship)
                    )

                Just ( Nothing, starship ) ->
                    ( { s | isFetching = True }
                    , Cmd.map
                        CreateStarshipBuildResult
                        (createStarshipBuild starship)
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
                                    , ships =
                                        Maybe.map
                                            (\ships ->
                                                { link = link, name = sb.name } :: ships
                                            )
                                            s.ships
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

        FlyStarshipGetResult r ->
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
                            , starshipBuild = Just ( Nothing, sb )
                            , flightStatus = Just (SelectingCrew emptyCrew)
                          }
                        , Cmd.none
                        )

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

        StatusUpdate sMsg ->
            case ( starshipBuild, flightStatus ) of
                ( Just ( _, sModel ), Just (Flying sStatus) ) ->
                    let
                        ( newStatus, cmd ) =
                            StatusEditor.update sModel sMsg sStatus
                    in
                    ( { s | flightStatus = Just (Flying newStatus) }, Cmd.map StatusUpdate cmd )

                _ ->
                    ( s, Cmd.none )

        CrewUpdate newCrew ->
            case ( starshipBuild, flightStatus ) of
                ( Just ( _, sModel ), Just (SelectingCrew _) ) ->
                    ( { s | flightStatus = Just (SelectingCrew newCrew) }, Cmd.none )

                _ ->
                    ( s, Cmd.none )

        AcceptCrew ->
            case ( starshipBuild, flightStatus ) of
                ( Just ( _, sModel ), Just (SelectingCrew crew) ) ->
                    ( { s | flightStatus = Just (Flying (StatusEditor.init sModel)) }, Cmd.none )

                _ ->
                    ( s, Cmd.none )

        Back ->
            case ( error, flightStatus ) of
                ( Just _, _ ) ->
                    ( { s | error = Nothing }, Cmd.none )

                ( Nothing, Just (Flying status) ) ->
                    -- TODO: This could lose saved progress!
                    ( { s | starshipBuild = Nothing, flightStatus = Just (SelectingCrew emptyCrew) }, Cmd.none )

                ( Nothing, _ ) ->
                    -- TODO: This could lose saved progress!
                    ( { s | starshipBuild = Nothing, flightStatus = Nothing }, Cmd.none )


view : Model -> Html Msg
view { starshipBuild, error, isFetching, shipName, ships, flightStatus } =
    div
        []
        (case ( ( starshipBuild, flightStatus ), error, isFetching ) of
            ( _, _, True ) ->
                [ text "..." ]

            ( _, Just e, _ ) ->
                [ text ("ERROR: " ++ e)
                , button [ onClick Back ] [ text "BACK" ]
                ]

            ( ( Just ( _, starship ), Just (SelectingCrew crew) ), _, _ ) ->
                [ Html.map CrewUpdate <| CrewEditor.view crew
                , button [ onClick AcceptCrew ] [ text "ACCEPT" ]
                , button [ onClick Back ] [ text "BACK" ]
                ]

            ( ( Just ( _, starship ), Just (Flying status) ), _, _ ) ->
                [ Html.map StatusUpdate <| StatusEditor.view starship status
                , button [ onClick Back ] [ text "BACK" ]
                ]

            ( ( Just ( _, starship ), Nothing ), _, _ ) ->
                [ Html.map StarshipUpdate <| StarshipEditor.view starship
                , button [ onClick SaveShip ] [ text "SAVE" ]
                , button [ onClick Back ] [ text "BACK" ]
                ]

            ( ( Nothing, _ ), _, _ ) ->
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
                                            [ label [] [ text (ship.name ++ ": ") ]
                                            , button
                                                [ onClick (GetShip ship.link)
                                                ]
                                                [ text "EDIT" ]
                                            , button [ onClick (FlyShip ship.link) ] [ text "FLY" ]
                                            ]
                                    )
                                    s

                        Nothing ->
                            [ div [] [ text "Ships Not Fetched" ] ]
                    )
                ]
        )
