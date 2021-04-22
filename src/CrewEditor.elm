module CrewEditor exposing (Crew, emptyCrew, view)

import Browser exposing (element)
import Crewmate exposing (Crewmate)
import CrewmateEditor exposing (init, update, view)
import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import InOrdDict exposing (InOrdDict)
import Platform.Cmd
import Task


type alias Crew =
    InOrdDict String Crewmate


type alias Model =
    { crew : Crew
    , waiting : Bool
    , error : Maybe String
    }


type Msg
    = UpdateCrew Crew
    | ClientMsg (Result String Crew)


emptyCrew : Crew
emptyCrew =
    InOrdDict.empty


init : Model
init =
    { crew = emptyCrew
    , waiting = False
    , error = Nothing
    }


type alias CrewClient =
    Crew -> Cmd (Result String Crew)


update : { a | updateCrew : CrewClient } -> Msg -> Model -> ( Model, Cmd Msg )
update { updateCrew } msg model =
    case msg of
        UpdateCrew new ->
            ( { model | waiting = True }, Cmd.map ClientMsg <| updateCrew new )

        ClientMsg (Ok crew) ->
            ( { model
                | waiting = False
                , crew = crew
              }
            , Cmd.none
            )

        ClientMsg (Err err) ->
            ( { model | waiting = False, error = Just err }, Cmd.none )


crewmateView : Crew -> String -> Crewmate -> List (Html Crew)
crewmateView crew name crewmate =
    [ label [] [ text "Name: " ]
    , input
        [ value name
        , onInput (\n -> InOrdDict.reKey name n crew)
        ]
        []
    , CrewmateEditor.view_ crewmate
        |> Html.map (\cm -> InOrdDict.insert name cm crew)
    ]


view_ : Crew -> List (Html Crew)
view_ crew =
    InOrdDict.foldl
        (\name crewmate h ->
            div [] (div [] [ text "=====" ] :: crewmateView crew name crewmate) :: h
        )
        []
        crew


view : Crew -> Html Crew
view crew =
    div []
        (div [] [ button [ onClick (InOrdDict.insert "" (.crewmate CrewmateEditor.init) crew) ] [ text "Add Crewmate" ] ]
            :: view_ crew
        )


mockClient : CrewClient
mockClient c =
    Task.perform identity (Task.succeed (Ok c))


main : Program () Model Msg
main =
    element
        { init = \_ -> ( init, Cmd.none )
        , update = update { updateCrew = mockClient }
        , view = .crew >> view >> Html.map UpdateCrew
        , subscriptions = \_ -> Sub.none
        }
