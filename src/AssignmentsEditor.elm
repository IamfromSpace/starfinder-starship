module AssignmentsEditor exposing (view)

import Assignments exposing (Assignment(..), Assignments)
import Html exposing (..)
import Html.Attributes exposing (disabled, selected, type_, value)
import Html.Events exposing (onInput)
import HtmlCmd exposing (HtmlCmd, element)
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


nameView : Bool -> Bool -> String -> Assignment -> Html Assignment
nameView canSelectCaptain canSelectPilot name assignment =
    div []
        [ label [] [ text (name ++ ": ") ]

        -- TODO: Prevent double captain/pilot selection
        -- Note:  It appears this only works reliably if you use _both_
        -- <select value> and <option selected>, yay!
        , select [ onInput fromValue, value (toValue assignment) ]
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


view :
    (( String, Assignment ) -> Assignments String -> Cmd (Assignments String))
    -> Assignments String
    -> HtmlCmd (Assignments String)
view setAssignments assignments =
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
                nameView (no Captain) (no Pilot) name assignment
                    |> Html.map
                        (\newAssignment -> setAssignments ( name, newAssignment ) assignments)
            )
            assignmentList
        )


pureSetAssignments : ( String, Assignment ) -> Assignments String -> Cmd (Assignments String)
pureSetAssignments ( name, newAssignment ) assignments =
    Assignments.move name newAssignment assignments
        |> Maybe.withDefault assignments
        |> Task.succeed
        |> Task.perform identity


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
        , view = view pureSetAssignments
        }
