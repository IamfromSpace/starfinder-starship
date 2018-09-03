module Link exposing (Link(..), link, toString, unlink)


type Link
    = Linked
    | Unlinked


toString : Link -> String
toString link_ =
    case link_ of
        Linked ->
            "Linked"

        Unlinked ->
            "Unlinked"


link : Link -> Link
link _ =
    Linked


unlink : Link -> Link
unlink _ =
    Unlinked
