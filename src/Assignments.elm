module Assignments exposing (Assignment(..), Assignments, allInEngineering, empty, move, toList)


type Assignment
    = Captain
    | Pilot
    | Engineer
    | ScienceOfficer
    | Gunner


type alias Assignments a =
    { captain : Maybe a
    , pilot : Maybe a
    , engineers : List a
    , scienceOfficers : List a
    , gunners : List a
    }


toList : Assignments a -> List ( a, Assignment )
toList { captain, pilot, engineers, scienceOfficers, gunners } =
    let
        addCaptain =
            Maybe.map ((::) << (\x -> ( x, Captain ))) captain
                |> Maybe.withDefault identity

        addPilot =
            Maybe.map ((::) << (\x -> ( x, Pilot ))) pilot
                |> Maybe.withDefault identity

        support =
            List.map (\x -> ( x, ScienceOfficer )) scienceOfficers
                ++ List.map (\x -> ( x, Engineer )) engineers
                ++ List.map (\x -> ( x, Gunner )) gunners
    in
    addCaptain (addPilot support)


move : a -> Assignment -> Assignments a -> Maybe (Assignments a)
move a to current =
    let
        withoutA =
            { captain =
                if current.captain == Just a then
                    Nothing

                else
                    current.captain
            , pilot =
                if current.pilot == Just a then
                    Nothing

                else
                    current.pilot
            , engineers = List.filter ((/=) a) current.engineers
            , scienceOfficers = List.filter ((/=) a) current.scienceOfficers
            , gunners = List.filter ((/=) a) current.gunners
            }
    in
    case to of
        Captain ->
            case withoutA.captain of
                Just _ ->
                    Nothing

                Nothing ->
                    Just { withoutA | captain = Just a }

        Pilot ->
            case withoutA.pilot of
                Just _ ->
                    Nothing

                Nothing ->
                    Just { withoutA | pilot = Just a }

        Engineer ->
            Just { withoutA | engineers = a :: withoutA.engineers }

        ScienceOfficer ->
            Just { withoutA | scienceOfficers = a :: withoutA.scienceOfficers }

        Gunner ->
            Just { withoutA | gunners = a :: withoutA.gunners }


empty : Assignments a
empty =
    { captain = Nothing
    , pilot = Nothing
    , engineers = []
    , scienceOfficers = []
    , gunners = []
    }


allInEngineering : List a -> Assignments a
allInEngineering engineeers =
    { captain = Nothing
    , pilot = Nothing
    , engineers = engineeers
    , scienceOfficers = []
    , gunners = []
    }


map : (a -> b) -> Assignments a -> Assignments b
map f a =
    { captain = Maybe.map f a.captain
    , pilot = Maybe.map f a.pilot
    , engineers = List.map f a.engineers
    , scienceOfficers = List.map f a.scienceOfficers
    , gunners = List.map f a.gunners
    }


traverseMaybe : (a -> Maybe b) -> Assignments a -> Maybe (Assignments b)
traverseMaybe f a =
    Maybe.map5
        (\c p es sos gs ->
            { captain = c
            , pilot = p
            , engineers = es
            , scienceOfficers = sos
            , gunners = gs
            }
        )
        (traverseMaybeMaybe f a.captain)
        (traverseMaybeMaybe f a.pilot)
        (traverseListMaybe f a.engineers)
        (traverseListMaybe f a.scienceOfficers)
        (traverseListMaybe f a.gunners)



-- TODO:  Move to a 'Traversable' module?


traverseListMaybe : (a -> Maybe b) -> List a -> Maybe (List b)
traverseListMaybe f xs =
    case xs of
        [] ->
            Just []

        h :: t ->
            Maybe.map2 (::) (f h) (traverseListMaybe f t)


traverseMaybeMaybe : (a -> Maybe b) -> Maybe a -> Maybe (Maybe b)
traverseMaybeMaybe =
    Maybe.map
