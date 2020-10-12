module Arc exposing (AnArc(..), Arc, all, concat, foldWithAnArc, foldr, getArc, getDegrees, liftA2, liftA3, map, mapWithAnArc, pure, pureWithAnArc, setArc, updateArc)


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


pureWithAnArc : (AnArc -> a) -> Arc a
pureWithAnArc f =
    { forward = f Forward
    , aft = f Aft
    , portSide = f Port
    , starboard = f Starboard
    }


map : (a -> b) -> Arc a -> Arc b
map fn arcs =
    { forward = fn arcs.forward
    , aft = fn arcs.aft
    , portSide = fn arcs.portSide
    , starboard = fn arcs.starboard
    }


mapWithAnArc : (AnArc -> a -> b) -> Arc a -> Arc b
mapWithAnArc fn arcs =
    { forward = fn Forward arcs.forward
    , aft = fn Aft arcs.aft
    , portSide = fn Port arcs.portSide
    , starboard = fn Starboard arcs.starboard
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


liftA2 : (a -> b -> c) -> Arc a -> Arc b -> Arc c
liftA2 op a b =
    { forward = op a.forward b.forward
    , aft = op a.aft b.aft
    , portSide = op a.portSide b.portSide
    , starboard = op a.starboard b.starboard
    }


liftA3 : (a -> b -> c -> d) -> Arc a -> Arc b -> Arc c -> Arc d
liftA3 op a b c =
    { forward = op a.forward b.forward c.forward
    , aft = op a.aft b.aft c.aft
    , portSide = op a.portSide b.portSide c.portSide
    , starboard = op a.starboard b.starboard c.starboard
    }


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


setArc : (a -> a) -> AnArc -> Arc a -> Arc a
setArc fn arc arcOf =
    case arc of
        Forward ->
            { arcOf | forward = fn arcOf.forward }

        Aft ->
            { arcOf | aft = fn arcOf.aft }

        Port ->
            { arcOf | portSide = fn arcOf.portSide }

        Starboard ->
            { arcOf | starboard = fn arcOf.starboard }
