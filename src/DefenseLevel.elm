module DefenseLevel exposing (DefenseLevel(..), decDefenseLevel, getDefenseLevelSeq, incDefenseLevel, toBonus, toManeuverabilityPenalty, toString, toTlPenalty)

-- TODO: Armor also decreases maneuverability


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


toTlPenalty : DefenseLevel -> Int
toTlPenalty defenseLevel =
    case defenseLevel of
        Mk1 ->
            0

        Mk2 ->
            0

        Mk3 ->
            0

        Mk4 ->
            0

        Mk5 ->
            -1

        Mk6 ->
            -1

        Mk7 ->
            -1

        Mk8 ->
            -1

        Mk9 ->
            -2

        Mk10 ->
            -2

        Mk11 ->
            -2

        Mk12 ->
            -3

        Mk13 ->
            -3

        Mk14 ->
            -3

        Mk15 ->
            -4


toBonus : DefenseLevel -> Int
toBonus defenseLevel =
    case defenseLevel of
        Mk1 ->
            1

        Mk2 ->
            2

        Mk3 ->
            3

        Mk4 ->
            4

        Mk5 ->
            5

        Mk6 ->
            6

        Mk7 ->
            7

        Mk8 ->
            8

        Mk9 ->
            9

        Mk10 ->
            10

        Mk11 ->
            11

        Mk12 ->
            12

        Mk13 ->
            13

        Mk14 ->
            14

        Mk15 ->
            15


toManeuverabilityPenalty : DefenseLevel -> Int
toManeuverabilityPenalty defenseLevel =
    case defenseLevel of
        Mk9 ->
            1

        Mk10 ->
            1

        Mk11 ->
            1

        Mk12 ->
            2

        Mk13 ->
            2

        Mk14 ->
            2

        Mk15 ->
            3

        _ ->
            0


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
