module Togglable exposing (Togglable, extract, map, meta, pure, toggle)

import Switch exposing (..)


type Togglable a
    = Togglable Switch a


toggle : Togglable a -> Togglable a
toggle togglable =
    let
        (Togglable switch a) =
            togglable
    in
    Togglable (Switch.toggle switch) a


map : (a -> b) -> Togglable a -> Togglable b
map f (Togglable switch a) =
    Togglable switch (f a)


extract : Togglable a -> a
extract (Togglable _ a) =
    a


meta : Togglable a -> Switch
meta (Togglable switch _) =
    switch


pure : a -> Togglable a
pure =
    Togglable On
