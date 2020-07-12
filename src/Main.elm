module Main exposing (main)

import App
import Browser exposing (element)
import CognitoClient
import Login
import Platform.Cmd exposing (Cmd)
import Platform.Sub exposing (Sub)
import Utils exposing (configStyle)


mkLoginConfig : String -> String -> Login.Config
mkLoginConfig region clientId =
    { login = CognitoClient.mkLogin region clientId
    , answerNewPasswordChallenge =
        CognitoClient.mkAnswerNewPasswordChallenge region
            clientId
    , refreshSession = CognitoClient.mkRefreshSession region clientId
    , cacheRefreshToken = \_ -> Cmd.none
    }


type alias Flags =
    { region : String
    , clientId : String
    , hostName : String
    }


main : Program Flags ( App.Config {}, App.Model ) App.Msg
main =
    element <|
        configStyle
            { init =
                \f ->
                    let
                        loginConfig =
                            mkLoginConfig f.region f.clientId

                        appConfig =
                            { loginInit = Login.mkInit loginConfig Nothing
                            , loginUpdate = Login.update loginConfig
                            , hostName = f.hostName
                            }
                    in
                    ( appConfig, App.init appConfig )
            , update =
                App.update
            , view =
                App.view
            , subscriptions =
                \_ -> Sub.none
            }
