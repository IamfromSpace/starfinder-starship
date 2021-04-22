module AssignmentsEditor exposing (Model, view)

import Assignments exposing (Assignment(..), Assignments)
import Html exposing (..)
import Html.Attributes exposing (disabled, selected, type_, value)
import Html.Events exposing (onInput)
import HtmlCmd exposing (HtmlCmd, element, lift)
import Set exposing (Set)
import Task


type alias Model =
    Assignments String


toValue : Assignment -> String
toValue a =
    case a of
        Captain ->
            "captain"

        Pilot ->
            "pilot"

        Engineer ->
            "engineer"

        ScienceOfficer ->
            "science officer"

        Gunner ->
            "gunner"


fromValue : String -> Assignment
fromValue str =
    case str of
        "captain" ->
            Captain

        "pilot" ->
            Pilot

        "science officer" ->
            ScienceOfficer

        "gunner" ->
            Gunner

        _ ->
            Engineer


nameView : Bool -> Bool -> Bool -> String -> Assignment -> Html Assignment
nameView isDisabled canSelectCaptain canSelectPilot name assignment =
    div []
        [ label [] [ text (name ++ ": ") ]

        -- TODO: Prevent double captain/pilot selection
        -- Note:  It appears this only works reliably if you use _both_
        -- <select value> and <option selected>, yay!
        , select [ disabled isDisabled, onInput fromValue, value (toValue assignment) ]
            [ option
                [ value (toValue Captain)
                , selected (assignment == Captain)
                , disabled (not canSelectCaptain)
                ]
                [ text "Captain" ]
            , option
                [ value (toValue Pilot)
                , selected (assignment == Pilot)
                , disabled (not canSelectPilot)
                ]
                [ text "Pilot" ]
            , option
                [ value (toValue ScienceOfficer)
                , selected (assignment == ScienceOfficer)
                ]
                [ text "Science Officer" ]
            , option
                [ value (toValue Engineer)
                , selected (assignment == Engineer)
                ]
                [ text "Engineer" ]
            , option
                [ value (toValue Gunner)
                , selected (assignment == Gunner)
                ]
                [ text "Gunner" ]
            ]
        ]


view : Bool -> Assignments String -> Html (Assignments String)
view isDisabled assignments =
    let
        assignmentList =
            Assignments.toList assignments

        no x =
            List.filter (\( _, a ) -> a == x) assignmentList
                |> List.length
                |> (==) 0
    in
    div []
        (List.map
            (\( name, assignment ) ->
                nameView isDisabled (no Captain) (no Pilot) name assignment
                    |> Html.map
                        (\newAssignment -> Maybe.withDefault assignments (Assignments.move name newAssignment assignments))
            )
            assignmentList
        )


main : Program () Model ( Model, Cmd Model )
main =
    element
        { init =
            \_ ->
                ( Assignments.allInEngineering
                    [ "Alice"
                    , "Bob"
                    , "Carol"
                    , "Dave"
                    , "Ed"
                    , "Frank"
                    ]
                , Cmd.none
                )
        , view = view False >> lift
        }
