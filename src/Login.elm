-- THIS MODULE IS COPIED FROM REACTION TIME PROJECT
-- MAKE CHANGES THERE OR MAKE THIS A DAMN LIBRARY


module Login exposing (Config, LoginUpdate, Model, Msg, mkInit, update, view)

import CognitoClient exposing (AnswerNewPasswordChallenge, Login, LoginResult(..), RefreshSession, TokenSet)
import Html exposing (Html, button, div, form, input, label, text)
import Html.Attributes exposing (disabled, style, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Error(..))
import Json.Decode exposing (field, string, succeed)
import Platform.Cmd exposing (Cmd)


mkInit : Config -> Maybe String -> ( Model, Cmd Msg )
mkInit { refreshSession } mRefreshToken =
    ( { email = ""
      , password = ""
      , passwordConfirmation = ""
      , session = Nothing
      , loading = mRefreshToken /= Nothing
      , lastError = Nothing
      }
    , case mRefreshToken of
        Just token ->
            Cmd.map tokensOrErrorToMsg <| refreshSession token

        Nothing ->
            Cmd.none
    )


type alias Model =
    { email : String
    , password : String
    , passwordConfirmation : String
    , session : Maybe String
    , loading : Bool
    , lastError : Maybe Error
    }


type Msg
    = NoOp
    | UpdateEmail String
    | UpdatePassword String
    | UpdatePasswordConfirmation String
    | RequestTokens
    | RequestNewPassword String
    | ReceiveError Error
    | ReceiveTokens TokenSet
    | ReceiveChallenge String


loginResponseToMsg : Result Error CognitoClient.LoginResult -> Msg
loginResponseToMsg result =
    case result of
        Ok (LoggedIn tokens) ->
            ReceiveTokens tokens

        Ok (NewPasswordRequired session) ->
            ReceiveChallenge session

        Err e ->
            ReceiveError e


tokensOrErrorToMsg : Result Error TokenSet -> Msg
tokensOrErrorToMsg result =
    case result of
        Ok tokens ->
            ReceiveTokens tokens

        Err e ->
            ReceiveError e


type alias LoginUpdate =
    Msg -> Model -> ( ( Model, Cmd Msg ), Maybe TokenSet )


type alias Config =
    { login : Login
    , answerNewPasswordChallenge : AnswerNewPasswordChallenge
    , refreshSession : RefreshSession
    , cacheRefreshToken : String -> Cmd ()
    }


validateNewPassword : String -> String -> Maybe String
validateNewPassword password passwordConfirmation =
    if String.length password < 12 then
        Just "Passwords must be 12 chars or more"

    else if password /= passwordConfirmation then
        Just "Passwords must match"

    else
        Nothing


update : Config -> LoginUpdate
update { login, cacheRefreshToken, answerNewPasswordChallenge } msg ({ email, password, passwordConfirmation, loading, lastError } as s) =
    case ( msg, loading ) of
        ( UpdateEmail new, False ) ->
            ( ( { s | email = new }, Cmd.none ), Nothing )

        ( UpdatePassword new, False ) ->
            ( ( { s | password = new }, Cmd.none ), Nothing )

        ( UpdatePasswordConfirmation new, False ) ->
            ( ( { s | passwordConfirmation = new }, Cmd.none ), Nothing )

        ( RequestTokens, False ) ->
            ( ( { s | loading = True, lastError = Nothing }
              , Cmd.map loginResponseToMsg <| login email password
              )
            , Nothing
            )

        ( RequestNewPassword session, False ) ->
            if validateNewPassword password passwordConfirmation == Nothing then
                ( ( { s | loading = True, lastError = Nothing }
                  , Cmd.map tokensOrErrorToMsg <| answerNewPasswordChallenge session email password
                  )
                , Nothing
                )

            else
                ( ( s, Cmd.none ), Nothing )

        ( ReceiveTokens tokens, True ) ->
            ( ( { email = ""
                , password = ""
                , passwordConfirmation = ""
                , loading = False
                , lastError = Nothing
                , session = Nothing
                }
              , Cmd.map (always NoOp) <| cacheRefreshToken tokens.refresh
              )
            , Just tokens
            )

        ( ReceiveChallenge session, True ) ->
            ( ( { s
                    | password = ""
                    , passwordConfirmation = ""
                    , loading = False
                    , lastError = Nothing
                    , session = Just session
                }
              , Cmd.none
              )
            , Nothing
            )

        ( ReceiveError e, True ) ->
            ( ( { s | loading = False, lastError = Just e }, Cmd.none ), Nothing )

        ( _, _ ) ->
            ( ( s, Cmd.none ), Nothing )


view : Model -> Html Msg
view { email, password, passwordConfirmation, loading, lastError, session } =
    form
        [ onSubmit <|
            case session of
                Nothing ->
                    RequestTokens

                Just s ->
                    RequestNewPassword s
        ]
        [ div []
            [ label [] [ text "Email" ]
            , input [ onInput UpdateEmail, type_ "email", value email, disabled loading ] []
            ]
        , div []
            [ label [] [ text "Password" ]
            , input [ onInput UpdatePassword, type_ "password", value password, disabled loading ] []
            ]
        , div []
            (case session of
                Nothing ->
                    []

                Just _ ->
                    [ label [] [ text "Confirm Password" ]
                    , input [ onInput UpdatePasswordConfirmation, type_ "password", value passwordConfirmation, disabled loading ] []
                    ]
            )
        , let
            passwordValidation =
                validateNewPassword password passwordConfirmation

            isValid =
                passwordValidation == Nothing
          in
          button [ disabled (loading || session /= Nothing && not isValid) ]
            [ text <|
                case session of
                    Nothing ->
                        "Login"

                    Just _ ->
                        case passwordValidation of
                            Just msg ->
                                msg

                            Nothing ->
                                "Update Password"
            ]
        , text <|
            case lastError of
                Nothing ->
                    ""

                Just (BadStatus x) ->
                    "Unexpected Status " ++ String.fromInt x

                Just NetworkError ->
                    "Is your internet connected?"

                Just Timeout ->
                    "Timeout trying to fetch tokens"

                Just (BadBody _) ->
                    "Unexpected response from auth service"

                Just (BadUrl _) ->
                    "Tried to reach an unreachable endpoint"
        ]
