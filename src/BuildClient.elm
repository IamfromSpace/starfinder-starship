module BuildClient exposing (CreateStarshipBuild, CreateStarshipBuildError(..), HttpClientError, createStarshipBuild, createStarshipBuildErrorToString, httpClientErrorToString)

import Arc exposing (Arc)
import DefenseLevel
import Dict
import ExpansionBay
import Http exposing (Expect, Response(..), expectStringResponse, header, request, stringBody)
import Json.Encode as E exposing (Value, encode)
import Link exposing (Link(..))
import LinkAndTogglable exposing (LinkAndTogglable(..))
import Starship exposing (BuildError, Starship)
import Switch exposing (Switch(..))
import Task
import Togglable exposing (Togglable(..))
import Weapon exposing (Range(..), Weapon)


type HttpClientError expected
    = Timeout
    | NetworkError
    | UnexpectedResponse
    | BadUrl String
    | ExpectedError expected


httpClientErrorToString : (expected -> String) -> HttpClientError expected -> String
httpClientErrorToString f e =
    case e of
        Timeout ->
            "Timeout"

        NetworkError ->
            "NetworkError"

        UnexpectedResponse ->
            "UnexpectedResponse"

        BadUrl s ->
            "BadUrl " ++ s

        ExpectedError ee ->
            "ExpectedError " ++ f ee


type CreateStarshipBuildError
    = AlreadyExists
    | Forbidden
    | BuildError (List BuildError)


createStarshipBuildErrorToString : CreateStarshipBuildError -> String
createStarshipBuildErrorToString e =
    case e of
        AlreadyExists ->
            "AlreadyExists"

        Forbidden ->
            "Forbidden"

        BuildError es ->
            "BuildError " ++ listToString (List.map Starship.buildErrorToString es)


listToString list =
    case list of
        [] ->
            "[]"

        x :: xs ->
            "[" ++ listToString_ xs


listToString_ list =
    case list of
        [] ->
            "]"

        x :: xs ->
            "," ++ listToString_ xs


responseToCreateBuildClientResult : Response String -> Result (HttpClientError CreateStarshipBuildError) String
responseToCreateBuildClientResult e =
    case e of
        GoodStatus_ { headers } _ ->
            case Dict.get "etag" (lowerCaseCommaJoin headers) of
                Just eTag ->
                    Ok eTag

                Nothing ->
                    Err UnexpectedResponse

        Timeout_ ->
            Err Timeout

        NetworkError_ ->
            Err NetworkError

        BadUrl_ x ->
            Err (BadUrl x)

        BadStatus_ { statusCode } _ ->
            if statusCode >= 500 then
                Err UnexpectedResponse

            else if statusCode == 403 then
                Err (ExpectedError Forbidden)

            else if statusCode == 409 then
                Err (ExpectedError AlreadyExists)
                -- TODO: Correctly Identify and parse BuildError

            else
                Err UnexpectedResponse


buildToXStarfinderStarshipBuildValue : Starship -> Value
buildToXStarfinderStarshipBuildValue sb =
    E.object
        [ ( "name", E.string sb.name )
        , ( "frame", E.string sb.frame.name )
        , ( "powerCoreUnits", E.int sb.powerCoreUnits )
        , ( "thrusters"
          , E.object
                [ ( "isOn", E.bool (Togglable.meta sb.thrusters == On) )
                , ( "toggled", E.int (Togglable.extract sb.thrusters) )
                ]
          )
        , ( "armor"
          , case sb.armor of
                Just x ->
                    E.string (DefenseLevel.toString x)

                Nothing ->
                    E.null
          )
        , ( "computer"
          , E.object
                [ ( "isOn", E.bool (Togglable.meta sb.computer == On) )
                , ( "toggled"
                  , let
                        { nodes, bonus } =
                            Togglable.extract sb.computer
                    in
                    E.object
                        [ ( "nodes", E.int nodes )
                        , ( "bonus", E.int bonus )
                        ]
                  )
                ]
          )
        , ( "crewQuarters"
          , E.string (Starship.crewQuartersToString sb.crewQuarters)
          )
        , ( "defensiveCountermeasures"
          , case sb.defensiveCountermeasures of
                Just x ->
                    E.object
                        [ ( "isOn", E.bool (Togglable.meta x == On) )
                        , ( "toggled", E.string (DefenseLevel.toString (Togglable.extract x)) )
                        ]

                Nothing ->
                    E.null
          )
        , ( "driftEngine"
          , case sb.driftEngine of
                Just x ->
                    E.string (Starship.driftEngineToString x)

                Nothing ->
                    E.null
          )
        , ( "expansionBays"
          , E.list (togglableToValue (ExpansionBay.toString >> E.string)) sb.expansionBays
          )
        , ( "sensors"
          , E.object
                [ ( "range"
                  , E.string
                        (case sb.sensors.range of
                            Short ->
                                "Short"

                            Medium ->
                                "Medium"

                            Long ->
                                "Long"
                        )
                  )
                , ( "bonus", E.int sb.sensors.bonus )
                ]
          )
        , ( "arcWeapons"
          , arcToValue (E.list linkAndTogglableWeaponToValue) sb.arcWeapons
          )
        , ( "turretWeapons"
          , E.list linkAndTogglableWeaponToValue sb.turretWeapons
          )
        , ( "shields", togglableToValue (.name >> E.string) sb.shields )
        ]


arcToValue : (a -> Value) -> Arc a -> Value
arcToValue f x =
    E.object
        [ ( "forward", f x.forward )
        , ( "aft", f x.aft )
        , ( "portSide", f x.portSide )
        , ( "starboard", f x.starboard )
        ]


togglableToValue : (a -> Value) -> Togglable a -> Value
togglableToValue f x =
    E.object
        [ ( "isOn", E.bool (Togglable.meta x == On) )
        , ( "toggled", f (Togglable.extract x) )
        ]


linkAndTogglableWeaponToValue : LinkAndTogglable Weapon -> Value
linkAndTogglableWeaponToValue x =
    let
        meta =
            LinkAndTogglable.meta x

        isLinked =
            meta.link == Linked

        isOn =
            meta.switch == On

        weapon =
            LinkAndTogglable.extract x
    in
    E.object
        [ ( "isOn", E.bool isOn )
        , ( "toggled"
          , E.object [ ( "isLinked", E.bool isLinked ), ( "name", E.string weapon.name ) ]
          )
        ]


type alias CreateStarshipBuild =
    String -> String -> String -> Starship -> Cmd (Result (HttpClientError CreateStarshipBuildError) String)


createStarshipBuild : CreateStarshipBuild
createStarshipBuild hostname userId token starshipBuild =
    request
        { method = "PUT"
        , headers =
            [ header "Authorization" ("Bearer " ++ token) ]

        -- TODO: Want to limit need for URL construction for clients
        , url = "https://" ++ hostname ++ "/resources/users/" ++ userId ++ "/builds/" ++ starshipBuild.name
        , body =
            starshipBuild
                |> buildToXStarfinderStarshipBuildValue
                |> encode 0
                |> stringBody "application/x.starfinder-starship-build+json"
        , expect = expectResponse responseToCreateBuildClientResult
        , timeout = Just 5000
        , tracker = Nothing
        }


expectResponse : (Response String -> Result x a) -> Expect (Result x a)
expectResponse =
    expectStringResponse identity



-- TODO: Would be nice to have a CI type


lowerCaseCommaJoin : Dict.Dict String String -> Dict.Dict String String
lowerCaseCommaJoin =
    Dict.foldl
        (\key nextValue ->
            Dict.update key
                (\currentValue ->
                    case currentValue of
                        Just cv ->
                            Just (cv ++ "," ++ nextValue)

                        Nothing ->
                            Just nextValue
                )
        )
        Dict.empty
