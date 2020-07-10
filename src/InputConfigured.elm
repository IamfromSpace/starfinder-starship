module InputConfigured exposing (Model, Msg(..), initialModel, update, view)

import Dict exposing (Dict)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)


type alias Model a =
    { config : Dict String String
    , inner : a
    }


initialModel : Dict String String -> (f -> ( model, Cmd msg )) -> f -> ( Model model, Cmd (Msg msg) )
initialModel config inner flags =
    let
        ( model, cmd ) =
            inner flags
    in
    ( { config = config
      , inner = model
      }
    , Cmd.map UpdateInner cmd
    )


type Msg a
    = UpdateConfig String String
    | UpdateInner a


update : (Dict String String -> config) -> (config -> msg -> model -> ( model, Cmd msg )) -> Msg msg -> Model model -> ( Model model, Cmd (Msg msg) )
update f u cMsg cModel =
    case cMsg of
        UpdateConfig k v ->
            ( { cModel | config = Dict.insert k v cModel.config }, Cmd.none )

        UpdateInner msg ->
            let
                ( model, cmd ) =
                    u (f cModel.config) msg cModel.inner
            in
            ( { cModel | inner = model }, Cmd.map UpdateInner cmd )


view : (Dict String String -> config) -> (config -> model -> Html msg) -> Model model -> Html (Msg msg)
view f innerView { config, inner } =
    let
        inputs =
            Dict.foldr
                (\k v list ->
                    div []
                        [ label [] [ text k ]
                        , input [ onInput (UpdateConfig k), value v ] []
                        ]
                        :: list
                )
                []
                config

        innerHtml =
            Html.map UpdateInner <| innerView (f config) inner
    in
    div [] (inputs ++ [ innerHtml ])


configSubs : (Dict String String -> config) -> (config -> model -> Sub msg) -> Model model -> Sub (Msg msg)
configSubs f innerSub { config, inner } =
    Sub.map UpdateInner <| innerSub (f config) inner
