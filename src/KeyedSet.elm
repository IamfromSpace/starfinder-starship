module KeyedSet exposing
    ( KeyedSet(..)
    , fromList
    , get
    , insert
    , keys
    , remove
    , toDict
    , values
    )

import Dict exposing (Dict)


type KeyedSet comparable v
    = KeyedSet (v -> comparable) (Dict comparable v)


insert : v -> KeyedSet comparable v -> KeyedSet comparable v
insert v (KeyedSet fn d) =
    KeyedSet fn <| Dict.insert (fn v) v d


remove : comparable -> KeyedSet comparable v -> KeyedSet comparable v
remove comparable (KeyedSet fn d) =
    KeyedSet fn <| Dict.remove comparable d


get : comparable -> KeyedSet comparable v -> Maybe v
get comparable (KeyedSet fn d) =
    Dict.get comparable d


fromList : (v -> comparable) -> List v -> KeyedSet comparable v
fromList fn =
    KeyedSet fn << Dict.fromList << List.map (\v -> ( fn v, v ))


keys : KeyedSet comparable v -> List comparable
keys (KeyedSet fn d) =
    List.map fn <| Dict.values d


values : KeyedSet comparable v -> List v
values (KeyedSet _ d) =
    Dict.values d


toDict : KeyedSet comparable v -> Dict comparable v
toDict (KeyedSet _ d) =
    d
