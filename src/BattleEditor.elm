module BattleEditor exposing (Model, Msg, init, main, update, view)

import Arc
import BattlePhase exposing (BattlePhase(..))
import Browser exposing (element)
import CombatPhase exposing (CombatPhase(..))
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, text)
import Html.Attributes as A
import Html.Events as E
import ShieldedFighter
import ShipAssets exposing (norikamaDropship)
import Starship exposing (Starship)
import StatusEditor
import Svg exposing (Svg)
import Svg.Attributes as SA


type alias Model =
    { statusEditors : Dict String ( Starship, StatusEditor.Model )
    , highlightedShip : String
    , currentRound : Int
    , currentPhase : BattlePhase
    }


init : Dict String Starship -> Model
init starships =
    { statusEditors = Dict.map (always (\s -> ( s, StatusEditor.init s ))) starships
    , highlightedShip = Maybe.withDefault "" (List.head (Dict.keys starships))
    , currentRound = 0
    , currentPhase = Assign
    }


type Msg
    = StatusEditorMsg String StatusEditor.Msg
    | NextPhase
    | SelectShip String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StatusEditorMsg id innerMsg ->
            Dict.get id model.statusEditors
                |> Maybe.map
                    (\( s, sem ) ->
                        let
                            ( newSem, cmds ) =
                                StatusEditor.update { starship = s, currentRound = model.currentRound, currentPhase = model.currentPhase } innerMsg sem

                            newSes =
                                Dict.insert id ( s, newSem ) model.statusEditors
                        in
                        ( { model | statusEditors = newSes }, Cmd.map (StatusEditorMsg id) cmds )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        NextPhase ->
            ( { model
                | currentRound =
                    model.currentRound
                        + (case model.currentPhase of
                            CP Gunnery ->
                                1

                            _ ->
                                0
                          )
                , currentPhase = BattlePhase.nextPhase model.currentPhase
              }
            , Cmd.none
            )

        SelectShip id ->
            ( { model | highlightedShip = id }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ A.style "display" "flex" ]
        [ if Dict.size model.statusEditors <= 1 then
            div [] []

          else
            Dict.toList model.statusEditors
                |> List.map
                    (\( name, ( s, se ) ) ->
                        div [ A.style "margin-right" "10px" ]
                            [ div [ A.style "font-size" "10px" ] [ text name ]
                            , Svg.svg
                                [ SA.height <| String.fromInt 100
                                , SA.width <| String.fromInt 100
                                , SA.viewBox <| "0 0 " ++ String.fromInt 100 ++ " " ++ String.fromInt 100
                                ]
                                [ --view : Starship -> Status -> (Arc.AnArc -> a) -> Arc.Arc (Maybe a) -> Arc.Arc (Maybe a) -> Float -> Svg a
                                  ShieldedFighter.view2 s
                                    --TODO: wee bit of coupling here
                                    se.status
                                    (Just (SelectShip name))
                                    (always (SelectShip name))
                                    (Arc.pure Nothing)
                                    (Arc.pure Nothing)
                                    100
                                ]
                            ]
                    )
                |> div []
        , div []
            [ text model.highlightedShip
            , Dict.get model.highlightedShip model.statusEditors
                |> Maybe.map
                    (\( starship, sem ) ->
                        Html.map (StatusEditorMsg model.highlightedShip) (StatusEditor.view { starship = starship, currentRound = model.currentRound, currentPhase = model.currentPhase } sem)
                    )
                |> Maybe.withDefault (div [] [ text "No ship highlighted" ])
            , button
                -- TODO: What about the _pre_ battle phase that is allotting???
                [ E.onClick NextPhase ]
                [ text ("PROCEED TO " ++ String.toUpper (BattlePhase.toString (BattlePhase.nextPhase model.currentPhase)) ++ " PHASE") ]
            ]
        ]


main : Program () Model Msg
main =
    element
        { init = \_ -> ( init (Dict.fromList [ ( "Alice", norikamaDropship ), ( "Bob", norikamaDropship ), ( "Carol", norikamaDropship ) ]), Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
