module Arc exposing (AnArc(..), Arc, all, concat, foldWithAnArc, foldr, getArc, getDegrees, map, pure, updateArc)


type alias Arc a =
    { forward : a
    , aft : a
    , portSide : a
    , starboard : a
    }


pure : a -> Arc a
pure a =
    { forward = a
    , aft = a
    , portSide = a
    , starboard = a
    }


map : (a -> b) -> Arc a -> Arc b
map fn arcs =
    { forward = fn arcs.forward
    , aft = fn arcs.aft
    , portSide = fn arcs.portSide
    , starboard = fn arcs.starboard
    }


foldr : (a -> b -> b) -> b -> Arc a -> b
foldr fn init arc =
    fn arc.forward init
        |> fn arc.aft
        |> fn arc.portSide
        |> fn arc.starboard


foldWithAnArc : (AnArc -> a -> b -> b) -> b -> Arc a -> b
foldWithAnArc fn init arc =
    fn Forward arc.forward init
        |> fn Aft arc.aft
        |> fn Port arc.portSide
        |> fn Starboard arc.starboard


all : Arc Bool -> Bool
all =
    foldr (&&) True


concat : Arc appendable -> appendable
concat arcs =
    arcs.forward
        ++ arcs.aft
        ++ arcs.portSide
        ++ arcs.starboard


type AnArc
    = Forward
    | Aft
    | Port
    | Starboard


getDegrees : AnArc -> Float
getDegrees arc =
    case arc of
        Forward ->
            0

        Aft ->
            180

        Port ->
            270

        Starboard ->
            90


updateArc : (a -> a) -> AnArc -> Arc a -> Arc a
updateArc fn arc arcOf =
    case arc of
        Forward ->
            { arcOf | forward = fn arcOf.forward }

        Aft ->
            { arcOf | aft = fn arcOf.aft }

        Port ->
            { arcOf | portSide = fn arcOf.portSide }

        Starboard ->
            { arcOf | starboard = fn arcOf.starboard }


getArc : AnArc -> Arc a -> a
getArc arc arcOf =
    case arc of
        Forward ->
            arcOf.forward

        Aft ->
            arcOf.aft

        Port ->
            arcOf.portSide

        Starboard ->
            arcOf.starboard
