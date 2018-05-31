module Linkable exposing (..)

import Link exposing (..)


type Linkable a
    = Linkable Link a


link : Linkable a -> Linkable a
link linkable =
    let
        (Linkable l a) =
            linkable
    in
        Linkable (Link.link l) a


unlink : Linkable a -> Linkable a
unlink linkable =
    let
        (Linkable l a) =
            linkable
    in
        Linkable (Link.unlink l) a


map : (a -> b) -> Linkable a -> Linkable b
map f (Linkable l a) =
    Linkable l (f a)
