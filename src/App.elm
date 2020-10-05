module App exposing (Config, Model, Msg, init, update, view)

import BuildClient as BC
import CognitoClient
import CreateAndEditStarshipBuild as CAESB
import Html exposing (Html)
import Login


type alias Model =
    { inner : CAESB.Model
    , login : Login.Model

    -- TODO: To _fully_ abstract out CognitoClient, Login just needs to rexport
    -- this as its own type.  Then this module has no idea that a CognitoClient
    -- exists at all.
    , tokens : Maybe CognitoClient.TokenSet
    }


init : Config a -> ( Model, Cmd Msg )
init { loginInit } =
    let
        ( login, cmd ) =
            loginInit
    in
    ( { inner = CAESB.initialModel
      , login = login
      , tokens = Nothing
      }
    , Cmd.map LoginMsg cmd
    )


type Msg
    = InnerMsg CAESB.Msg
    | LoginMsg Login.Msg


type alias Config a =
    { a
        | loginUpdate : Login.LoginUpdate
        , loginInit : ( Login.Model, Cmd Login.Msg )
        , createStarshipBuild : String -> BC.CreateStarshipBuild
        , getStarshipBuild : String -> BC.GetStarshipBuild
        , updateStarshipBuild : String -> BC.UpdateStarshipBuild
        , getStarshipBuilds : String -> BC.GetStarshipBuilds
    }


update :
    Config a
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
update config msg model =
    case msg of
        InnerMsg iMsg ->
            case model.tokens of
                Just { id } ->
                    let
                        ( iModel, cmd ) =
                            CAESB.update
                                { createStarshipBuild = config.createStarshipBuild id
                                , getStarshipBuild = config.getStarshipBuild id
                                , updateStarshipBuild = config.updateStarshipBuild id
                                , getStarshipBuilds = config.getStarshipBuilds id
                                }
                                iMsg
                                model.inner
                    in
                    ( { model | inner = iModel }, Cmd.map InnerMsg cmd )

                Nothing ->
                    ( model, Cmd.none )

        LoginMsg lMsg ->
            let
                ( ( lModel, cmd ), tokens ) =
                    config.loginUpdate
                        lMsg
                        model.login
            in
            ( { model
                | login = lModel
                , tokens = tokens
              }
            , Cmd.map LoginMsg cmd
            )


view : Model -> Html Msg
view { tokens, login, inner } =
    case tokens of
        Nothing ->
            Html.map LoginMsg <| Login.view login

        Just _ ->
            Html.map InnerMsg <| CAESB.view inner
