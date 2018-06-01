module Arc exposing (..)


type alias Arc a =
    { forward : a
    , aft : a
    , portSide : a
    , starboard : a
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


all : Arc Bool -> Bool
all =
    foldr (&&) True


concat : Arc appendable -> appendable
concat arcs =
    arcs.forward
        ++ arcs.aft
        ++ arcs.portSide
        ++ arcs.starboard
