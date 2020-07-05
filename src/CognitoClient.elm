-- THIS MODULE IS COPIED FROM REACTION TIME PROJECT
-- MAKE CHANGES THERE OR MAKE THIS A DAMN LIBRARY


module CognitoClient exposing (AnswerNewPasswordChallenge, Login, LoginResult(..), RefreshSession, TokenSet, mkAnswerNewPasswordChallenge, mkLogin, mkRefreshSession)

import Http exposing (Error, expectJson, header, request, stringBody)
import Json.Decode as D exposing (Decoder)
import Json.Encode exposing (Value, encode, object, string)



-- This module is based on the reference js implementation:
-- https://github.com/aws-amplify/amplify-js/blob/master/packages/amazon-cognito-identity-js/src/CognitoUser.js
-- Examples of it in use:
-- https://docs.aws.amazon.com/cognito/latest/developerguide/using-amazon-cognito-user-identity-pools-javascript-examples.html


type alias TokenSet =
    { id : String
    , access : String
    , refresh : String
    }


type alias LoginInfo =
    { username : String
    , password : String
    }


loginInfoToValue : LoginInfo -> Value
loginInfoToValue { username, password } =
    object [ ( "USERNAME", string username ), ( "PASSWORD", string password ) ]


type alias LoginBody =
    { authFlow : String
    , clientId : String
    , authParameters : LoginInfo
    }


loginBodyToValue : LoginBody -> Value
loginBodyToValue { authFlow, clientId, authParameters } =
    object
        [ ( "AuthFlow", string authFlow )
        , ( "ClientId", string clientId )
        , ( "AuthParameters", loginInfoToValue authParameters )
        ]


type
    LoginResult
    -- For now we just want the Tokens
    = LoggedIn TokenSet
    | NewPasswordRequired String


decodeIdToken : Decoder String
decodeIdToken =
    D.at [ "AuthenticationResult", "IdToken" ] D.string


decodeAccessToken : Decoder String
decodeAccessToken =
    D.at [ "AuthenticationResult", "AccessToken" ] D.string


decodeRefreshToken : Decoder String
decodeRefreshToken =
    D.at [ "AuthenticationResult", "RefreshToken" ] D.string


decodeTokens : Decoder TokenSet
decodeTokens =
    D.map3 TokenSet decodeIdToken decodeAccessToken decodeRefreshToken


decodeTokensOnRefresh : String -> Decoder TokenSet
decodeTokensOnRefresh refreshToken =
    D.map2 (\i a -> TokenSet i a refreshToken) decodeIdToken decodeAccessToken


decodeLoginResponse : Decoder LoginResult
decodeLoginResponse =
    D.oneOf
        [ D.map LoggedIn <| decodeTokens
        , D.field "ChallengeName" D.string
            |> D.andThen
                (\challengeName ->
                    if challengeName == "NEW_PASSWORD_REQUIRED" then
                        D.map NewPasswordRequired <| D.field "Session" D.string

                    else
                        D.fail "Only the new password challenge is supported!"
                )
        ]


type alias Login =
    String -> String -> Cmd (Result Error LoginResult)


mkLogin : String -> String -> Login
mkLogin region clientId username password =
    request
        { method = "POST"
        , headers =
            [ header "X-Amz-Target" "AWSCognitoIdentityProviderService.InitiateAuth" ]
        , url = "https://cognito-idp." ++ region ++ ".amazonaws.com"
        , body =
            stringBody "application/x-amz-json-1.1" <|
                encode 0 <|
                    loginBodyToValue
                        { authFlow = "USER_PASSWORD_AUTH"
                        , clientId = clientId
                        , authParameters = { username = username, password = password }
                        }
        , expect = expectJson identity decodeLoginResponse
        , timeout = Just 5000
        , tracker = Nothing
        }


type alias AnswerNewPasswordChallenge =
    String -> String -> String -> Cmd (Result Error TokenSet)


mkAnswerNewPasswordChallenge : String -> String -> AnswerNewPasswordChallenge
mkAnswerNewPasswordChallenge region clientId session username password =
    request
        { method = "POST"
        , headers =
            [ header "X-Amz-Target" "AWSCognitoIdentityProviderService.RespondToAuthChallenge" ]
        , url = "https://cognito-idp." ++ region ++ ".amazonaws.com"
        , body =
            stringBody "application/x-amz-json-1.1" <|
                encode 0 <|
                    object
                        [ ( "ChallengeName", string "NEW_PASSWORD_REQUIRED" )
                        , ( "ClientId", string clientId )
                        , ( "ChallengeResponses"
                          , object
                                [ ( "USERNAME", string username )
                                , ( "NEW_PASSWORD", string password )
                                ]
                          )
                        , ( "Session", string session )
                        ]
        , expect = expectJson identity decodeTokens
        , timeout = Just 5000
        , tracker = Nothing
        }


type alias RefreshSession =
    String -> Cmd (Result Error TokenSet)


mkRefreshSession : String -> String -> RefreshSession
mkRefreshSession region clientId refreshToken =
    request
        { method = "POST"
        , headers =
            [ header "X-Amz-Target" "AWSCognitoIdentityProviderService.InitiateAuth" ]
        , url = "https://cognito-idp." ++ region ++ ".amazonaws.com"
        , body =
            stringBody "application/x-amz-json-1.1" <|
                encode 0 <|
                    object
                        [ ( "AuthFlow", string "REFRESH_TOKEN_AUTH" )
                        , ( "ClientId", string clientId )
                        , ( "AuthParameters"
                          , object [ ( "REFRESH_TOKEN", string refreshToken ) ]
                          )
                        ]
        , expect = expectJson identity (decodeTokensOnRefresh refreshToken)
        , timeout = Just 5000
        , tracker = Nothing
        }
