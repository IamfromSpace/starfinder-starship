module DefenseLevel exposing (DefenseLevel(..), decDefenseLevel, getDefenseLevelSeq, incDefenseLevel, toString)


type DefenseLevel
    = Mk1
    | Mk2
    | Mk3
    | Mk4
    | Mk5
    | Mk6
    | Mk7
    | Mk8
    | Mk9
    | Mk10
    | Mk11
    | Mk12
    | Mk13
    | Mk14
    | Mk15


toString : DefenseLevel -> String
toString defenseLevel =
    case defenseLevel of
        Mk1 ->
            "Mk1"

        Mk2 ->
            "Mk2"

        Mk3 ->
            "Mk3"

        Mk4 ->
            "Mk4"

        Mk5 ->
            "Mk5"

        Mk6 ->
            "Mk6"

        Mk7 ->
            "Mk7"

        Mk8 ->
            "Mk8"

        Mk9 ->
            "Mk9"

        Mk10 ->
            "Mk10"

        Mk11 ->
            "Mk11"

        Mk12 ->
            "Mk12"

        Mk13 ->
            "Mk13"

        Mk14 ->
            "Mk14"

        Mk15 ->
            "Mk15"


getDefenseLevelSeq : DefenseLevel -> ( Maybe DefenseLevel, Maybe DefenseLevel )
getDefenseLevelSeq defenseLevel =
    case defenseLevel of
        Mk1 ->
            ( Nothing, Just Mk2 )

        Mk2 ->
            ( Just Mk1, Just Mk3 )

        Mk3 ->
            ( Just Mk2, Just Mk4 )

        Mk4 ->
            ( Just Mk3, Just Mk5 )

        Mk5 ->
            ( Just Mk4, Just Mk6 )

        Mk6 ->
            ( Just Mk5, Just Mk7 )

        Mk7 ->
            ( Just Mk6, Just Mk8 )

        Mk8 ->
            ( Just Mk7, Just Mk9 )

        Mk9 ->
            ( Just Mk8, Just Mk10 )

        Mk10 ->
            ( Just Mk9, Just Mk11 )

        Mk11 ->
            ( Just Mk10, Just Mk12 )

        Mk12 ->
            ( Just Mk11, Just Mk13 )

        Mk13 ->
            ( Just Mk12, Just Mk14 )

        Mk14 ->
            ( Just Mk13, Just Mk15 )

        Mk15 ->
            ( Just Mk14, Nothing )


incDefenseLevel : DefenseLevel -> Maybe DefenseLevel
incDefenseLevel =
    getDefenseLevelSeq >> Tuple.second


decDefenseLevel : DefenseLevel -> Maybe DefenseLevel
decDefenseLevel =
    getDefenseLevelSeq >> Tuple.first
