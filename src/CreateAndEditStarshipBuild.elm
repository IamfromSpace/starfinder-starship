module CreateAndEditStarshipBuild exposing (Config, Model, Msg(..), initialModel, update, view)

import Arc
import BattleEditor
import BuildClient exposing (CreateStarshipBuild, CreateStarshipBuildError, GetStarshipBuild, GetStarshipBuildError, GetStarshipBuilds, GetStarshipBuildsError(..), HttpClientError, Link, StarshipBuildLink, UpdateStarshipBuild, UpdateStarshipBuildError, createStarshipBuild, createStarshipBuildErrorToString, getStarshipBuild, getStarshipBuildErrorToString, getStarshipBuilds, getStarshipBuildsErrorToString, httpClientErrorToString, updateStarshipBuild, updateStarshipBuildErrorToString)
import Dict
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes as A exposing (disabled, value)
import Html.Events as E exposing (onClick, onInput)
import InOrdDict exposing (toDict)
import InputConfigured as IC
import KeyedSet as KS
import Platform.Cmd exposing (Cmd)
import ShipAssets
import Starship exposing (Starship)
import StarshipEditor
import Task
import Togglable
import Weapon


initialModel : Model
initialModel =
    { starshipBuild = Nothing
    , battle = Nothing
    , error = Nothing
    , isFetching = False
    , shipName = ""
    , ships = Nothing
    , selected = []
    }


type alias Model =
    -- TODO: Store the previous Starship Build state so we can detect if it's
    -- actually changed
    { starshipBuild : Maybe ( Maybe ( String, Link ), Starship )
    , battle : Maybe BattleEditor.Model
    , error : Maybe String
    , isFetching : Bool
    , shipName : String
    , ships : Maybe (List StarshipBuildLink)

    -- So irritating how it's impossible to implement comparable.
    -- This should really be a Set.
    , selected : List Link
    }


type Msg
    = CreateStarshipBuildResult (Result (HttpClientError CreateStarshipBuildError) ( String, Link ))
    | GetStarshipBuildResult Link (Result (HttpClientError GetStarshipBuildError) ( String, Starship ))
    | GetStarshipBuildsResult (Result (HttpClientError GetStarshipBuildsError) (List StarshipBuildLink))
    | UpdateStarshipBuildResult (Result (HttpClientError UpdateStarshipBuildError) String)
    | StartBattleResult (Result (HttpClientError GetStarshipBuildError) (List Starship))
    | CreateShip
    | SaveShip
    | GetShip Link
    | GetShips
    | ToggleShipSelected Link
    | StartBattle
    | SetShipName String
    | StarshipUpdate StarshipEditor.Msg
    | BattleUpdate BattleEditor.Msg
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
update { getStarshipBuild, getStarshipBuilds, createStarshipBuild, updateStarshipBuild } msg ({ starshipBuild, error, isFetching, shipName, battle, selected } as s) =
    case msg of
        GetShip link ->
            ( { s | isFetching = True }
            , Cmd.map
                (GetStarshipBuildResult link)
                (Task.attempt identity <| getStarshipBuild link)
            )

        GetShips ->
            ( { s | isFetching = True }
            , Cmd.map
                GetStarshipBuildsResult
                getStarshipBuilds
            )

        CreateShip ->
            ( { s | starshipBuild = Just ( Nothing, StarshipEditor.init ) }, Cmd.none )

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

        ToggleShipSelected link ->
            ( { s
                | selected =
                    if List.member link selected then
                        List.filter ((/=) link) selected

                    else
                        link :: selected
              }
            , Cmd.none
            )

        StartBattle ->
            if List.length selected < 1 then
                ( s, Cmd.none )

            else
                ( { s | isFetching = True }
                , List.map (getStarshipBuild >> Task.map Tuple.second) selected
                    |> Task.sequence
                    |> Task.attempt identity
                    |> Cmd.map StartBattleResult
                )

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

        StartBattleResult r ->
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

                    Ok sbs ->
                        ( { s
                            | isFetching = False
                            , starshipBuild = Nothing

                            -- TODO: In a lot of ways, it would make more sense
                            -- to use the name just for rendering, and use the
                            -- Link as the Dict key.  However, Link can't be
                            -- comparable, so without making it not opaque,
                            -- that doesn't work.
                            , battle = Just (BattleEditor.init (Dict.fromList (List.map (\sb -> ( sb.name, sb )) sbs)))
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

        BattleUpdate bMsg ->
            case battle of
                Just battleModel ->
                    let
                        ( newBattle, cmd ) =
                            BattleEditor.update bMsg battleModel
                    in
                    ( { s | battle = Just newBattle }, Cmd.map BattleUpdate cmd )

                _ ->
                    ( s, Cmd.none )

        Back ->
            case ( error, battle ) of
                ( Just _, _ ) ->
                    ( { s | error = Nothing }, Cmd.none )

                ( Nothing, _ ) ->
                    -- TODO: This could lose saved progress!
                    ( { s | starshipBuild = Nothing, battle = Nothing }, Cmd.none )


view : Model -> Html Msg
view { starshipBuild, error, isFetching, shipName, ships, battle, selected } =
    div
        []
        (case ( ( starshipBuild, battle ), error, isFetching ) of
            ( _, _, True ) ->
                [ text "..." ]

            ( _, Just e, _ ) ->
                [ text ("ERROR: " ++ e)
                , button [ onClick Back ] [ text "BACK" ]
                ]

            ( ( _, Just battleModel ), _, _ ) ->
                [ Html.map BattleUpdate <| BattleEditor.view battleModel ]

            ( ( Just ( _, starship ), _ ), _, _ ) ->
                [ Html.map StarshipUpdate <| StarshipEditor.view starship
                , button [ onClick SaveShip ] [ text "SAVE" ]
                , button [ onClick Back ] [ text "BACK" ]
                ]

            _ ->
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
                                            , input
                                                [ A.type_ "checkbox"
                                                , A.checked
                                                    (List.member
                                                        ship.link
                                                        selected
                                                    )
                                                , E.onClick (ToggleShipSelected ship.link)
                                                ]
                                                []
                                            ]
                                    )
                                    s

                        Nothing ->
                            [ div [] [ text "Ships Not Fetched" ] ]
                    )
                , button [ disabled (List.length selected < 1), onClick StartBattle ] [ text "START BATTLE" ]
                ]
        )
