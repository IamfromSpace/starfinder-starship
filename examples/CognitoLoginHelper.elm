module Main exposing (LoginState(..), Model, Msg(..), initialModel, update, view)

import Browser exposing (element)
import CognitoClient
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (disabled, value)
import Html.Events exposing (onClick, onInput)
import Http
import Login
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)


initialModel : Model
initialModel =
    { clientId = ""
    , awsRegion = "us-west-2"
    , refreshToken = ""
    , loginState = NotLoggedIn
    }


type alias Model =
    { clientId : String
    , awsRegion : String
    , refreshToken : String
    , loginState : LoginState
    }


type LoginState
    = NotLoggedIn
    | Refreshing
    | LoggingIn Login.Model
    | LoggedIn CognitoClient.TokenSet


type Msg
    = SetClientId String
    | SetAwsRegion String
    | LoginMsg Login.Msg
    | StartLogin
    | SetRefreshToken String
    | Refresh
    | RefreshResult (Result Http.Error CognitoClient.TokenSet)


mkLoginConfig region clientId =
    { login = CognitoClient.mkLogin region clientId
    , answerNewPasswordChallenge = CognitoClient.mkAnswerNewPasswordChallenge region clientId
    , refreshSession = CognitoClient.mkRefreshSession region clientId
    , cacheRefreshToken = always Cmd.none
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ clientId, awsRegion, loginState, refreshToken } as s) =
    case ( msg, loginState ) of
        ( SetClientId ci, NotLoggedIn ) ->
            ( { s | clientId = ci }, Cmd.none )

        ( SetAwsRegion ar, NotLoggedIn ) ->
            ( { s | awsRegion = ar }, Cmd.none )

        ( SetRefreshToken rt, NotLoggedIn ) ->
            ( { s | refreshToken = rt }, Cmd.none )

        ( StartLogin, NotLoggedIn ) ->
            let
                ( m, cmds ) =
                    Login.mkInit
                        (mkLoginConfig awsRegion clientId)
                        Nothing
            in
            ( { s | loginState = LoggingIn m }, Cmd.map LoginMsg cmds )

        ( Refresh, NotLoggedIn ) ->
            let
                cmd =
                    (mkLoginConfig awsRegion clientId).refreshSession refreshToken
            in
            ( { s | loginState = Refreshing }, Cmd.map RefreshResult cmd )

        ( RefreshResult (Ok x), Refreshing ) ->
            ( { s | loginState = LoggedIn x }, Cmd.none )

        ( RefreshResult (Err x), Refreshing ) ->
            ( { s | loginState = NotLoggedIn }, Cmd.none )

        ( LoginMsg m, LoggingIn x ) ->
            let
                ( ( next, cmds ), maybeNotifications ) =
                    Login.update (mkLoginConfig awsRegion clientId) m x
            in
            case maybeNotifications of
                Nothing ->
                    ( { s | loginState = LoggingIn next }, Cmd.map LoginMsg cmds )

                Just tokens ->
                    ( { s | loginState = LoggedIn tokens }, Cmd.map LoginMsg cmds )

        _ ->
            ( s, Cmd.none )


view : Model -> Html Msg
view ({ clientId, awsRegion, refreshToken, loginState } as s) =
    div
        []
        (case loginState of
            LoggingIn m ->
                [ Html.map LoginMsg (Login.view m) ]

            NotLoggedIn ->
                [ div [] [ label [] [ text "Client Id" ], input [ onInput SetClientId, value clientId ] [] ]
                , div [] [ label [] [ text "AWS Region" ], input [ onInput SetAwsRegion, value awsRegion ] [] ]
                , button [ onClick StartLogin ] [ text "Go To Login" ]
                , div [] [ label [] [ text "Refresh Token" ], input [ onInput SetRefreshToken, value refreshToken ] [] ]
                , button [ onClick Refresh ] [ text "Refresh" ]
                ]

            Refreshing ->
                [ text "Refreshing..." ]

            LoggedIn tokenSet ->
                [ div [] [ text ("Identity Token: " ++ tokenSet.id) ]
                , div [] [ text ("Access Token: " ++ tokenSet.access) ]
                , div [] [ text ("Refresh Token: " ++ tokenSet.refresh) ]
                ]
        )


main : Program () Model Msg
main =
    element
        { init =
            \_ -> ( initialModel, Cmd.none )
        , update = update
        , view =
            view
        , subscriptions = \_ -> Sub.none
        }
