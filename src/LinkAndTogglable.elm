module LinkAndTogglable exposing (LinkAndTogglable(..), Meta, extract, link, map, meta, pure, toggle, unlink)

import Link exposing (..)
import Switch exposing (..)


type alias Meta =
    { link : Link
    , switch : Switch
    }


type LinkAndTogglable a
    = LinkAndTogglable Meta a


link : LinkAndTogglable a -> LinkAndTogglable a
link (LinkAndTogglable x y) =
    LinkAndTogglable { x | link = Link.link x.link } y


unlink : LinkAndTogglable a -> LinkAndTogglable a
unlink (LinkAndTogglable x y) =
    LinkAndTogglable { x | link = Link.unlink x.link } y


toggle : LinkAndTogglable a -> LinkAndTogglable a
toggle (LinkAndTogglable x y) =
    LinkAndTogglable { x | switch = Switch.toggle x.switch } y


map : (a -> b) -> LinkAndTogglable a -> LinkAndTogglable b
map f (LinkAndTogglable x y) =
    LinkAndTogglable x (f y)


extract : LinkAndTogglable a -> a
extract (LinkAndTogglable _ a) =
    a


meta : LinkAndTogglable a -> Meta
meta (LinkAndTogglable m _) =
    m


pure : a -> LinkAndTogglable a
pure =
    LinkAndTogglable { link = Unlinked, switch = On }
