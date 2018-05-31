module Link exposing (..)


type Link
    = Linked
    | Unlinked


link : Link -> Link
link _ =
    Linked


unlink : Link -> Link
unlink _ =
    Unlinked
