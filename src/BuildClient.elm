module BuildClient exposing (CreateStarshipBuild, CreateStarshipBuildError(..), GetStarshipBuild, GetStarshipBuildError(..), GetStarshipBuilds, GetStarshipBuildsError(..), HttpClientError, Link, StarshipBuildLink, UpdateStarshipBuild, UpdateStarshipBuildError(..), createStarshipBuild, createStarshipBuildErrorToString, getStarshipBuild, getStarshipBuildErrorToString, getStarshipBuilds, getStarshipBuildsErrorToString, httpClientErrorToString, makeMockGetStarshipBuild, makeMockUpdateStarshipBuild, mockCreateStarshipBuild, mockGetStarshipBuilds, updateStarshipBuild, updateStarshipBuildErrorToString)

import Arc exposing (Arc)
import Computer exposing (Computer)
import DefenseLevel exposing (DefenseLevel(..))
import Dict
import ExpansionBay exposing (ExpansionBay(..))
import Frame exposing (Frame)
import Http exposing (Expect, Response(..), emptyBody, expectStringResponse, header, request, stringBody, stringResolver, task)
import Json.Decode as D exposing (Decoder, decodeString)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E exposing (Value, encode)
import Jwt
import KeyedSet as KS
import Link exposing (Link(..))
import LinkAndTogglable exposing (LinkAndTogglable(..))
import ShipAssets exposing (frames, shields, weapons)
import Starship exposing (BuildError, CrewQuarters(..), DriftEngine(..), Sensor, Shields, Starship)
import Switch exposing (Switch(..))
import Task exposing (Task)
import Togglable exposing (Togglable(..))
import Weapon exposing (Range(..), Weapon)



-- Opaque type to ensure links aren't inspected or manipulated


type Link
    = Link String



-- TODO: Ideally the JWT is totally opaque and not inspected


subDecoder : Decoder String
subDecoder =
    D.field "sub" D.string



-- TODO: This feels like a classic case of "compression" rather than a well
-- thought through and reusable  abstraction


withSubFromTokenTask : String -> (String -> Task (HttpClientError a) b) -> Task (HttpClientError a) b
withSubFromTokenTask token f =
    case Jwt.decodeToken subDecoder token of
        Ok sub ->
            f sub

        Err _ ->
            Task.fail (UnexpectedResponse "JWT Token is invalid")


withSubFromToken : String -> (String -> Cmd (Result (HttpClientError a) b)) -> Cmd (Result (HttpClientError a) b)
withSubFromToken token f =
    case Jwt.decodeToken subDecoder token of
        Ok sub ->
            f sub

        Err _ ->
            Task.perform identity <|
                Task.succeed (Err (UnexpectedResponse "JWT Token is invalid"))


type HttpClientError expected
    = Timeout
    | NetworkError
    | UnexpectedResponse String
    | BadUrl String
    | ExpectedError expected


httpClientErrorToString : (expected -> String) -> HttpClientError expected -> String
httpClientErrorToString f e =
    case e of
        Timeout ->
            "Timeout"

        NetworkError ->
            "NetworkError"

        UnexpectedResponse s ->
            "UnexpectedResponse " ++ s

        BadUrl s ->
            "BadUrl " ++ s

        ExpectedError ee ->
            "ExpectedError " ++ f ee


type CreateStarshipBuildError
    = AlreadyExists
    | ForbiddenC
      -- TODO: Invalid component reference (aka. bad weapon/frame name)
    | BuildErrorC (List BuildError)


createStarshipBuildErrorToString : CreateStarshipBuildError -> String
createStarshipBuildErrorToString e =
    case e of
        AlreadyExists ->
            "AlreadyExists"

        ForbiddenC ->
            "Forbidden"

        BuildErrorC es ->
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



-- TODO: We don't want to thread the link we came up with, we want to use a link returned by the service


responseToCreateBuildClientResult : Link -> Response String -> Result (HttpClientError CreateStarshipBuildError) ( String, Link )
responseToCreateBuildClientResult link e =
    case e of
        GoodStatus_ { headers } _ ->
            case Dict.get "etag" (lowerCaseCommaJoin headers) of
                Just eTag ->
                    Ok ( eTag, link )

                Nothing ->
                    Err (UnexpectedResponse "Missing ETag")

        Timeout_ ->
            Err Timeout

        NetworkError_ ->
            Err NetworkError

        BadUrl_ x ->
            Err (BadUrl x)

        BadStatus_ { statusCode } _ ->
            if statusCode >= 500 then
                Err (UnexpectedResponse "5XX Status code")

            else if statusCode == 403 then
                Err (ExpectedError ForbiddenC)

            else if statusCode == 409 then
                Err (ExpectedError AlreadyExists)
                -- TODO: Correctly Identify and parse BuildError

            else
                Err (UnexpectedResponse "Unexpected (non-500) status code")


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
    Starship -> Cmd (Result (HttpClientError CreateStarshipBuildError) ( String, Link ))


createStarshipBuild : String -> String -> CreateStarshipBuild
createStarshipBuild hostname token starshipBuild =
    withSubFromToken token
        (\userId ->
            let
                -- TODO: Want to limit need for URL construction for clients
                url =
                    "https://" ++ hostname ++ "/resources/users/" ++ userId ++ "/builds/" ++ starshipBuild.name
            in
            request
                { method = "PUT"
                , headers =
                    [ header "Authorization" ("Bearer " ++ token) ]
                , url = url
                , body =
                    starshipBuild
                        |> buildToXStarfinderStarshipBuildValue
                        |> encode 0
                        |> stringBody "application/x.starfinder-starship-build+json"
                , expect = expectResponse <| responseToCreateBuildClientResult <| Link url
                , timeout = Just 5000
                , tracker = Nothing
                }
        )


type GetStarshipBuildError
    = ForbiddenG
    | DoesNotExistG


getStarshipBuildErrorToString : GetStarshipBuildError -> String
getStarshipBuildErrorToString x =
    case x of
        ForbiddenG ->
            "Forbidden"

        DoesNotExistG ->
            "DoesNotExist"


xStarfinderStarshipBuildToBuildDecoder : Decoder Starship
xStarfinderStarshipBuildToBuildDecoder =
    D.succeed Starship
        |> required "name" D.string
        |> required "frame" frameDecoder
        |> required "powerCoreUnits" D.int
        |> required "thrusters" (togglableDecoder D.int)
        |> optional "armor" (D.map Just defenseLevelDecoder) Nothing
        |> required "computer" (togglableDecoder computerDecoder)
        |> required "crewQuarters" crewQuartersDecoder
        |> optional "defensiveCountermeasures" (D.map Just (togglableDecoder defenseLevelDecoder)) Nothing
        |> optional "driftEngine" (D.map Just driftEngineDecoder) Nothing
        |> required "expansionBays" (D.list (togglableDecoder expansionBayDecoder))
        |> required "sensors" sensorsDecoder
        |> required "arcWeapons" (arcDecoder (D.list linkAndTogglableWeaponDecoder))
        |> required "turretWeapons" (D.list linkAndTogglableWeaponDecoder)
        |> required "shields" (togglableDecoder shieldsDecoder)


frameDecoder : Decoder Frame
frameDecoder =
    D.andThen
        (\x ->
            case KS.get x frames of
                Just frame ->
                    D.succeed frame

                Nothing ->
                    D.fail (x ++ " is not a valid frame!")
        )
        D.string


togglableDecoder : Decoder a -> Decoder (Togglable a)
togglableDecoder d =
    D.map2 Togglable
        (D.field "isOn"
            (D.map
                (\x ->
                    if x then
                        On

                    else
                        Off
                )
                D.bool
            )
        )
        (D.field "toggled" d)


defenseLevelDecoder : Decoder DefenseLevel
defenseLevelDecoder =
    D.andThen
        (\x ->
            case x of
                "Mk1" ->
                    D.succeed Mk1

                "Mk2" ->
                    D.succeed Mk2

                "Mk3" ->
                    D.succeed Mk3

                "Mk4" ->
                    D.succeed Mk4

                "Mk5" ->
                    D.succeed Mk5

                "Mk6" ->
                    D.succeed Mk6

                "Mk7" ->
                    D.succeed Mk7

                "Mk8" ->
                    D.succeed Mk8

                "Mk9" ->
                    D.succeed Mk9

                "Mk10" ->
                    D.succeed Mk10

                "Mk11" ->
                    D.succeed Mk11

                "Mk12" ->
                    D.succeed Mk12

                "Mk13" ->
                    D.succeed Mk13

                "Mk14" ->
                    D.succeed Mk14

                "Mk15" ->
                    D.succeed Mk15

                _ ->
                    D.fail "Invalid DefenseLevel"
        )
        D.string


computerDecoder : Decoder Computer
computerDecoder =
    D.succeed Computer
        |> required "nodes" D.int
        |> required "bonus" D.int


crewQuartersDecoder : Decoder CrewQuarters
crewQuartersDecoder =
    D.andThen
        (\x ->
            case x of
                "Common" ->
                    D.succeed Common

                "GoodQuarters" ->
                    D.succeed GoodQuarters

                "Luxurious" ->
                    D.succeed Luxurious

                cq ->
                    D.fail ("Invalid CrewQuarters: " ++ cq)
        )
        D.string


driftEngineDecoder : Decoder DriftEngine
driftEngineDecoder =
    D.andThen
        (\x ->
            case x of
                "Basic" ->
                    D.succeed Basic

                "Booster" ->
                    D.succeed Booster

                "Major" ->
                    D.succeed Major

                "Superior" ->
                    D.succeed Superior

                "Ultra" ->
                    D.succeed Ultra

                de ->
                    D.fail ("Invalid DriftEngine: " ++ de)
        )
        D.string


expansionBayDecoder : Decoder ExpansionBay
expansionBayDecoder =
    D.andThen
        (\x ->
            case x of
                "ArcaneLaboratory" ->
                    D.succeed ArcaneLaboratory

                "CargoHold" ->
                    D.succeed CargoHold

                "EscapePods" ->
                    D.succeed EscapePods

                "HangarBay" ->
                    D.succeed HangarBay

                "LifeBoats" ->
                    D.succeed LifeBoats

                "MedicalBay" ->
                    D.succeed MedicalBay

                "PassengerSeating" ->
                    D.succeed PassengerSeating

                "PowerCoreHousing" ->
                    D.succeed PowerCoreHousing

                "RecreationSuiteGym" ->
                    D.succeed RecreationSuiteGym

                "RecreationSuiteTrivedDen" ->
                    D.succeed RecreationSuiteTrivedDen

                "RecreationSuiteHac" ->
                    D.succeed RecreationSuiteHac

                "ScienceLab" ->
                    D.succeed ScienceLab

                "SealedEnvironmentChamber" ->
                    D.succeed SealedEnvironmentChamber

                "ShuttleBay" ->
                    D.succeed ShuttleBay

                "SynthesisBay" ->
                    D.succeed SynthesisBay

                "TechWorkshop" ->
                    D.succeed TechWorkshop

                eb ->
                    case String.split " " eb of
                        [ "SmugglerCompartment", s ] ->
                            case String.toInt s of
                                Just i ->
                                    D.succeed (SmugglerCompartment i)

                                Nothing ->
                                    D.fail "Invalid SmugglerCompartment"

                        _ ->
                            D.fail ("Invalid ExpansionBay: " ++ eb)
        )
        D.string


sensorsDecoder : Decoder Sensor
sensorsDecoder =
    D.succeed Sensor
        |> required "range" rangeDecoder
        |> required "bonus" D.int


rangeDecoder : Decoder Range
rangeDecoder =
    D.andThen
        (\x ->
            case x of
                "Short" ->
                    D.succeed Short

                "Medium" ->
                    D.succeed Medium

                "Long" ->
                    D.succeed Long

                r ->
                    D.fail ("Invalid Range: " ++ r)
        )
        D.string


arcDecoder : Decoder a -> Decoder (Arc a)
arcDecoder d =
    D.succeed Arc
        |> required "forward" d
        |> required "aft" d
        |> required "portSide" d
        |> required "starboard" d


linkAndTogglableWeaponDecoder : Decoder (LinkAndTogglable Weapon)
linkAndTogglableWeaponDecoder =
    D.map2 (\switch ( link, weapon ) -> LinkAndTogglable { switch = switch, link = link } weapon)
        (D.field "isOn"
            (D.map
                (\x ->
                    if x then
                        On

                    else
                        Off
                )
                D.bool
            )
        )
        (D.field "toggled"
            (D.map2 (\x y -> ( x, y ))
                (D.field "isLinked"
                    (D.map
                        (\x ->
                            if x then
                                Linked

                            else
                                Unlinked
                        )
                        D.bool
                    )
                )
                (D.field "name" weaponDecoder)
            )
        )


weaponDecoder : Decoder Weapon
weaponDecoder =
    D.andThen
        (\x ->
            -- TODO: Want to actually look this up in a Map
            case KS.get x weapons of
                Just weapon ->
                    D.succeed weapon

                Nothing ->
                    D.fail (x ++ " is not a valid weapon!")
        )
        D.string


shieldsDecoder : Decoder Shields
shieldsDecoder =
    D.andThen
        (\x ->
            case KS.get x shields of
                Just s ->
                    D.succeed s

                Nothing ->
                    D.fail (x ++ " is not a valid shields!")
        )
        D.string


responseToGetBuildClientResult : Response String -> Result (HttpClientError GetStarshipBuildError) ( String, Starship )
responseToGetBuildClientResult r =
    case r of
        GoodStatus_ { headers } body ->
            case ( Dict.get "etag" (lowerCaseCommaJoin headers), decodeString xStarfinderStarshipBuildToBuildDecoder body ) of
                ( Just eTag, Ok starship ) ->
                    Ok ( eTag, starship )

                ( _, Err e ) ->
                    Err (UnexpectedResponse ("Could not decode body :" ++ D.errorToString e))

                ( Nothing, _ ) ->
                    Err (UnexpectedResponse "Missing ETag")

        Timeout_ ->
            Err Timeout

        NetworkError_ ->
            Err NetworkError

        BadUrl_ x ->
            Err (BadUrl x)

        BadStatus_ { statusCode } _ ->
            if statusCode >= 500 then
                Err (UnexpectedResponse "5XX Status Code")

            else if statusCode == 403 then
                Err (ExpectedError ForbiddenG)

            else if statusCode == 404 then
                Err (ExpectedError DoesNotExistG)

            else
                Err (UnexpectedResponse "Unexpected (non-5XX) status code")


type alias GetStarshipBuild =
    Link -> Task (HttpClientError GetStarshipBuildError) ( String, Starship )


getStarshipBuild : String -> GetStarshipBuild
getStarshipBuild token (Link url) =
    withSubFromTokenTask token
        (\userId ->
            task
                { method = "GET"
                , headers =
                    [ header "Authorization" ("Bearer " ++ token) ]
                , url = url
                , body = emptyBody
                , resolver = stringResolver responseToGetBuildClientResult
                , timeout = Just 5000
                }
        )


type UpdateStarshipBuildError
    = ForbiddenU
    | DoesNotExistU
    | ETagMismatch ( String, Starship )
      -- TODO: There are a couple types that can be decoded
    | IllegalChange
    | BuildErrorU (List BuildError)


updateStarshipBuildErrorToString : UpdateStarshipBuildError -> String
updateStarshipBuildErrorToString x =
    case x of
        ForbiddenU ->
            "Forbidden"

        DoesNotExistU ->
            "DoesNotExist"

        ETagMismatch ( eTag, _ ) ->
            "ETagMismatch " ++ eTag

        IllegalChange ->
            "IllegalChange"

        BuildErrorU es ->
            "BuildError " ++ listToString (List.map Starship.buildErrorToString es)


responseToUpdateBuildClientResult : Response String -> Result (HttpClientError UpdateStarshipBuildError) String
responseToUpdateBuildClientResult r =
    case r of
        GoodStatus_ { headers } _ ->
            case Dict.get "etag" (lowerCaseCommaJoin headers) of
                Just eTag ->
                    Ok eTag

                Nothing ->
                    Err (UnexpectedResponse "Missing ETag")

        Timeout_ ->
            Err Timeout

        NetworkError_ ->
            Err NetworkError

        BadUrl_ x ->
            Err (BadUrl x)

        BadStatus_ { statusCode, headers } body ->
            if statusCode >= 500 then
                Err (UnexpectedResponse "5XX Status Code")

            else if statusCode == 403 then
                Err (ExpectedError ForbiddenU)

            else if statusCode == 404 then
                Err (ExpectedError DoesNotExistU)

            else if statusCode == 409 then
                Err (ExpectedError IllegalChange)
                -- TODO: Correctly Identify and parse BuildError

            else if statusCode == 412 then
                case ( Dict.get "etag" (lowerCaseCommaJoin headers), decodeString xStarfinderStarshipBuildToBuildDecoder body ) of
                    ( Just eTag, Ok starship ) ->
                        Err (ExpectedError (ETagMismatch ( eTag, starship )))

                    ( _, Err e ) ->
                        Err (UnexpectedResponse ("412 had invalid body :" ++ D.errorToString e))

                    ( Nothing, _ ) ->
                        Err (UnexpectedResponse "412 did not include ETag")

            else
                Err (UnexpectedResponse "Unexpected (non-5XX) status code")


type alias UpdateStarshipBuild =
    -- TODO: ETag and Token could be opaque too
    Link -> String -> Starship -> Cmd (Result (HttpClientError UpdateStarshipBuildError) String)


updateStarshipBuild : String -> UpdateStarshipBuild
updateStarshipBuild token (Link url) eTag starshipBuild =
    withSubFromToken token
        (\userId ->
            request
                { method = "PUT"
                , headers =
                    [ header "Authorization" ("Bearer " ++ token)
                    , header "If-Match" eTag
                    ]
                , url = url
                , body =
                    starshipBuild
                        |> buildToXStarfinderStarshipBuildValue
                        |> encode 0
                        |> stringBody "application/x.starfinder-starship-build+json"
                , expect = expectResponse responseToUpdateBuildClientResult
                , timeout = Just 5000
                , tracker = Nothing
                }
        )


type alias StarshipBuildLink =
    { name : String
    , link : Link
    }


starshipBuildLinkDecoder : Decoder StarshipBuildLink
starshipBuildLinkDecoder =
    D.succeed StarshipBuildLink
        |> required "name" D.string
        |> required "link" (D.map Link D.string)


type GetStarshipBuildsError
    = ForbiddenGs


getStarshipBuildsErrorToString : GetStarshipBuildsError -> String
getStarshipBuildsErrorToString x =
    case x of
        ForbiddenGs ->
            "Forbidden"


responseToGetBuildsClientResult : Response String -> Result (HttpClientError GetStarshipBuildsError) (List StarshipBuildLink)
responseToGetBuildsClientResult r =
    case r of
        GoodStatus_ _ body ->
            case decodeString (D.list starshipBuildLinkDecoder) body of
                Ok linkList ->
                    Ok linkList

                Err _ ->
                    Err (UnexpectedResponse "Could not decode body!")

        Timeout_ ->
            Err Timeout

        NetworkError_ ->
            Err NetworkError

        BadUrl_ x ->
            Err (BadUrl x)

        BadStatus_ { statusCode, headers } body ->
            if statusCode >= 500 then
                Err (UnexpectedResponse "5XX Status Code")

            else if statusCode == 403 then
                Err (ExpectedError ForbiddenGs)

            else
                Err (UnexpectedResponse "Unexpected 4XX")


type alias GetStarshipBuilds =
    Cmd (Result (HttpClientError GetStarshipBuildsError) (List StarshipBuildLink))


getStarshipBuilds : String -> String -> GetStarshipBuilds
getStarshipBuilds hostname token =
    withSubFromToken token
        (\userId ->
            request
                { method = "GET"
                , headers =
                    [ header "Authorization" ("Bearer " ++ token)
                    ]

                -- TODO: Want to limit need for URL construction for clients
                , url = "https://" ++ hostname ++ "/resources/users/" ++ userId ++ "/builds"
                , body = emptyBody
                , expect = expectResponse responseToGetBuildsClientResult
                , timeout = Just 5000
                , tracker = Nothing
                }
        )


expectResponse : (Response String -> Result x a) -> Expect (Result x a)
expectResponse =
    expectStringResponse identity



-- TODO: Would be nice to have a CI type


lowerCaseCommaJoin : Dict.Dict String String -> Dict.Dict String String
lowerCaseCommaJoin =
    Dict.foldl
        (\key nextValue ->
            Dict.update (String.toLower key)
                (\currentValue ->
                    case currentValue of
                        Just cv ->
                            Just (cv ++ "," ++ nextValue)

                        Nothing ->
                            Just nextValue
                )
        )
        Dict.empty



--TODO: We need this to be defined in the same module to keep Link opaque everywhere else.


mockCreateStarshipBuild : CreateStarshipBuild
mockCreateStarshipBuild _ =
    Task.perform identity <|
        Task.succeed <|
            Ok ( "eTag", Link "hey" )


makeMockGetStarshipBuild : Starship -> GetStarshipBuild
makeMockGetStarshipBuild defaultShip link =
    case link of
        Link "hey" ->
            Task.succeed ( "eTag", { defaultShip | name = "hey" } )

        Link "sup" ->
            Task.succeed ( "eTag", { defaultShip | name = "sup" } )

        _ ->
            Task.fail <| ExpectedError DoesNotExistG


makeMockUpdateStarshipBuild : Starship -> UpdateStarshipBuild
makeMockUpdateStarshipBuild defaultShip link eTag ship =
    Task.perform identity <|
        Task.succeed <|
            case ( link, eTag ) of
                ( Link "hey", "eTag" ) ->
                    Ok "eTag"

                ( Link "hey", _ ) ->
                    Err <| ExpectedError <| ETagMismatch ( "eTag", defaultShip )

                _ ->
                    Err <| ExpectedError DoesNotExistU


mockGetStarshipBuilds : GetStarshipBuilds
mockGetStarshipBuilds =
    Task.perform identity <|
        Task.succeed <|
            Ok
                [ { link = Link "hey", name = "hey" }
                , { link = Link "sup", name = "sup" }
                ]
