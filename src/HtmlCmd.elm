module HtmlCmd exposing (HtmlCmd, element, lift, map)

import Browser
import Html exposing (..)
import Task


type alias HtmlCmd a =
    Html (Cmd a)


map : (a -> b) -> HtmlCmd a -> HtmlCmd b
map =
    Html.map << Cmd.map


lift : Html a -> HtmlCmd a
lift =
    Html.map (Task.succeed >> Task.perform identity)


element : { view : m -> Html (Cmd m), init : f -> ( m, Cmd m ) } -> Program f m ( m, Cmd m )
element { init, view } =
    Browser.element
        { init =
            \f ->
                let
                    ( m, cmd ) =
                        init f
                in
                ( m, Cmd.map (\x -> ( x, Cmd.none )) cmd )
        , update =
            \( model, cmd ) _ ->
                ( model, Cmd.map (\x -> ( x, Cmd.none )) cmd )
        , view = \m -> Html.map (\x -> ( m, x )) (view m)

        -- TODO:
        , subscriptions = \_ -> Sub.none
        }
