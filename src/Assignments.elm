module Assignments exposing (Assignments)


type alias Assignments a =
    { captain : Maybe a
    , pilot : Maybe a
    , engineers : List a
    , scienceOfficers : List a
    , gunners : List a
    }


mapAssignments : (a -> b) -> Assignments a -> Assignments b
mapAssignments f a =
    { captain = Maybe.map f a.captain
    , pilot = Maybe.map f a.pilot
    , engineers = List.map f a.engineers
    , scienceOfficers = List.map f a.scienceOfficers
    , gunners = List.map f a.gunners
    }


traverseAssignmentsMaybe : (a -> Maybe b) -> Assignments a -> Maybe (Assignments b)
traverseAssignmentsMaybe f a =
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
