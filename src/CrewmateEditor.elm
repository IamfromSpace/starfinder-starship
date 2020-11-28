module CrewmateEditor exposing (view)

import Browser exposing (element)
import Crewmate exposing (Crewmate)
import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)
import Platform.Cmd
import Task


type alias Model =
    { etag : Maybe String
    , crewmate : Crewmate
    , waiting : Bool
    , error : Maybe String
    }


type Msg
    = UpdateCrewmate Crewmate
    | ClientMsg (Result String ( Crewmate, String ))


init : Model
init =
    { etag = Nothing
    , crewmate = Crewmate.init
    , waiting = False
    , error = Nothing
    }


type alias CrewmateClient =
    ( Crewmate, Maybe String ) -> Cmd (Result String ( Crewmate, String ))


update : { a | updateCrewmate : CrewmateClient } -> Msg -> Model -> ( Model, Cmd Msg )
update { updateCrewmate } msg model =
    case msg of
        UpdateCrewmate new ->
            ( { model | waiting = True }, Cmd.map ClientMsg <| updateCrewmate ( new, model.etag ) )

        ClientMsg (Ok ( crewmate, etag )) ->
            ( { model
                | waiting = False
                , etag = Just etag
                , crewmate = crewmate
              }
            , Cmd.none
            )

        ClientMsg (Err err) ->
            ( { model | waiting = False, error = Just err }, Cmd.none )


incDecView : String -> (Int -> a) -> Int -> Html a
incDecView desc f x =
    div []
        [ label [] [ text desc ]
        , input
            [ value (String.fromInt x)
            , onInput (String.toInt >> Maybe.withDefault 0 >> f)
            , type_ "number"
            ]
            []
        ]


view : Model -> Html Msg
view =
    Html.map UpdateCrewmate << view_ << .crewmate


view_ : Crewmate -> Html Crewmate
view_ crewmate =
    div []
        [ incDecView
            "Level: "
            (\x -> { crewmate | level = x })
            crewmate.level
        , incDecView
            "Base Attack Bonus: "
            (\x -> { crewmate | baseAttackBonus = x })
            crewmate.baseAttackBonus
        , incDecView
            "Dexterity Bonus: "
            (\x -> { crewmate | dexterityBonus = x })
            crewmate.dexterityBonus
        , incDecView
            "Reflex Save: "
            (\x -> { crewmate | reflexSave = x })
            crewmate.reflexSave
        , incDecView
            "Piloting Skill RANKS: "
            (\x -> { crewmate | pilotingRanks = x })
            crewmate.pilotingRanks
        , incDecView
            "Engineering Skill RANKS: "
            (\x -> { crewmate | engineeringRanks = x })
            crewmate.engineeringRanks
        , incDecView
            "Computers Skill RANKS: "
            (\x -> { crewmate | computersRanks = x })
            crewmate.computersRanks
        , incDecView
            "Piloting Skill Total Bonus: "
            (\x -> { crewmate | pilotingSkillBonus = x })
            crewmate.pilotingSkillBonus
        , incDecView
            "Engineering Skill Total Bonus: "
            (\x -> { crewmate | engineeringSkillBonus = x })
            crewmate.engineeringSkillBonus
        , incDecView
            "Computers Skill Total Bonus: "
            (\x -> { crewmate | computersSkillBonus = x })
            crewmate.computersSkillBonus
        , incDecView
            "Diplomacy Skill Total Bonus: "
            (\x -> { crewmate | diplomacySkillBonus = x })
            crewmate.diplomacySkillBonus
        , incDecView
            "Intimidate Skill Total Bonus: "
            (\x -> { crewmate | intimidateSkillBonus = x })
            crewmate.intimidateSkillBonus
        , incDecView
            "Bluff Skill Total Bonus: "
            (\x -> { crewmate | bluffSkillBonus = x })
            crewmate.bluffSkillBonus

        -- TODO: Stamina and HP, still not really sure these are even part of
        -- the Crewmate (maybe maxes are, but are they needed?  Can you heal?)
        ]


mockClient : CrewmateClient
mockClient ( c, _ ) =
    Task.perform identity (Task.succeed (Ok ( c, "MOCK" )))


main : Program () Model Msg
main =
    element
        { init = \_ -> ( init, Cmd.none )
        , update = update { updateCrewmate = mockClient }
        , view = view
        , subscriptions = \_ -> Sub.none
        }
