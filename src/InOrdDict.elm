module InOrdDict exposing (InOrdDict, empty, foldl, fromDict, insert, reKey, toDict)

import Dict exposing (Dict)


toDict : InOrdDict comparable v -> Dict comparable v
toDict =
    .dict >> Dict.map (\_ ( _, v ) -> v)


fromDict : Dict comparable v -> InOrdDict comparable v
fromDict =
    List.foldl (\( k, v ) -> insert k v) empty
        << Dict.toList


type alias InOrdDict k v =
    { dict : Dict k ( Int, v )
    , lastIndex : Int
    , insertionOrder : Dict Int ( k, v )
    }


empty : InOrdDict k v
empty =
    { dict = Dict.empty
    , lastIndex = 0
    , insertionOrder = Dict.empty
    }


insert : comparable -> v -> InOrdDict comparable v -> InOrdDict comparable v
insert k v iod =
    case Dict.get k iod.dict of
        Just ( i, _ ) ->
            { iod
                | dict = Dict.insert k ( i, v ) iod.dict
                , insertionOrder = Dict.insert i ( k, v ) iod.insertionOrder
            }

        Nothing ->
            let
                i =
                    iod.lastIndex
            in
            { iod
                | dict = Dict.insert k ( i, v ) iod.dict
                , insertionOrder = Dict.insert i ( k, v ) iod.insertionOrder
                , lastIndex = i + 1
            }



-- This is a weird one, where we rename a key, but don't change the order.


reKey : comparable -> comparable -> InOrdDict comparable v -> InOrdDict comparable v
reKey old new iod =
    case Dict.get old iod.dict of
        Just ( i, v ) ->
            { iod
                | dict = Dict.insert new ( i, v ) <| Dict.remove old iod.dict
                , insertionOrder = Dict.insert i ( new, v ) iod.insertionOrder
            }

        Nothing ->
            iod


foldl : (k -> v -> a -> a) -> a -> InOrdDict k v -> a
foldl f a iod =
    Dict.foldl (\_ ( k, v ) -> f k v) a iod.insertionOrder
