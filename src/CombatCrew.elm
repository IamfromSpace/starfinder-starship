module CombatCrew exposing (CombatCrew, audaciousGambit, backOff, barrelRoll, demandSource, demandTarget, empty, encourageSource, encourageTarget, evade, flipAndBurn, flyby, fullPower, getAcModifier, getCrew, getTlModifier, maneuver, movingSpeechSource, ordersSource, ordersTarget, slide, tauntSource, turnInPlace, updateCrew)

import Assignments exposing (Assignments)
import Crewmate exposing (Crewmate)
import CrewmateStatus exposing (CrewmateStatus)
import Dict exposing (Dict)


type alias CombatCrew a =
    Dict a ( Crewmate, CrewmateStatus )


empty : CombatCrew a
empty =
    Dict.empty


getCrew : CombatCrew comparable -> Dict comparable Crewmate
getCrew =
    Dict.map (always Tuple.first)


updateCrew : Dict comparable Crewmate -> CombatCrew comparable -> CombatCrew comparable
updateCrew cmDict cc =
    Dict.foldr
        (\k newCm newCC ->
            case Dict.get k cc of
                Just ( _, cs ) ->
                    Dict.insert k ( newCm, cs ) newCC

                Nothing ->
                    Dict.insert k ( newCm, CrewmateStatus.init ) newCC
        )
        Dict.empty
        cmDict


tupleHelper : (Crewmate -> Maybe Int) -> (CrewmateStatus -> a -> Maybe ( CrewmateStatus, Int )) -> ( Crewmate, CrewmateStatus ) -> a -> Maybe ( ( Crewmate, CrewmateStatus ), Int )
tupleHelper f g ( cm, cms ) r =
    case ( f cm, g cms r ) of
        ( Just crewBonus, Just ( newCrewStatus, crewStatusBonus ) ) ->
            Just
                ( ( cm, newCrewStatus )
                , crewBonus + crewStatusBonus
                )

        _ ->
            Nothing


idHelper : comparable -> (Crewmate -> Maybe Int) -> (CrewmateStatus -> a -> Maybe ( CrewmateStatus, Int )) -> CombatCrew comparable -> a -> Maybe ( CombatCrew comparable, Int )
idHelper id f g crew r =
    Dict.get id crew
        |> Maybe.andThen (\x -> tupleHelper f g x r)
        |> Maybe.map (\( y, b ) -> ( Dict.insert id y crew, b ))


pilotCheckHelper : (Crewmate -> Maybe Int) -> (CrewmateStatus -> { a | assignments : Assignments comparable } -> Maybe ( CrewmateStatus, Int )) -> CombatCrew comparable -> { a | assignments : Assignments comparable } -> Maybe ( CombatCrew comparable, Int )
pilotCheckHelper f g crew ({ assignments } as r) =
    assignments.pilot
        |> Maybe.andThen (\id -> idHelper id f g crew r)


maneuver : CombatCrew comparable -> { a | currentRound : Int, assignments : Assignments comparable } -> Maybe ( CombatCrew comparable, Int )
maneuver =
    pilotCheckHelper (Crewmate.maneuver >> Just) CrewmateStatus.maneuver


backOff : CombatCrew comparable -> { a | currentRound : Int, assignments : Assignments comparable } -> Maybe ( CombatCrew comparable, Int )
backOff =
    pilotCheckHelper (Crewmate.backOff >> Just) CrewmateStatus.backOff


barrelRoll : CombatCrew comparable -> { a | currentRound : Int, assignments : Assignments comparable } -> Maybe ( CombatCrew comparable, Int )
barrelRoll =
    pilotCheckHelper (Crewmate.barrelRoll >> Just) CrewmateStatus.barrelRoll


evade : CombatCrew comparable -> { a | currentRound : Int, assignments : Assignments comparable } -> Maybe ( CombatCrew comparable, Int )
evade =
    pilotCheckHelper (Crewmate.evade >> Just) CrewmateStatus.evade


flipAndBurn : CombatCrew comparable -> { a | currentRound : Int, assignments : Assignments comparable } -> Maybe ( CombatCrew comparable, Int )
flipAndBurn =
    pilotCheckHelper (Crewmate.flipAndBurn >> Just) CrewmateStatus.flipAndBurn


flyby : CombatCrew comparable -> { a | currentRound : Int, assignments : Assignments comparable } -> Maybe ( CombatCrew comparable, Int )
flyby =
    pilotCheckHelper (Crewmate.flyby >> Just) CrewmateStatus.flyby


slide : CombatCrew comparable -> { a | currentRound : Int, assignments : Assignments comparable } -> Maybe ( CombatCrew comparable, Int )
slide =
    pilotCheckHelper (Crewmate.slide >> Just) CrewmateStatus.slide


turnInPlace : CombatCrew comparable -> { a | currentRound : Int, assignments : Assignments comparable } -> Maybe ( CombatCrew comparable, Int )
turnInPlace =
    pilotCheckHelper (Crewmate.turnInPlace >> Just) CrewmateStatus.turnInPlace


fullPower : CombatCrew comparable -> { a | currentRound : Int, assignments : Assignments comparable } -> Maybe ( CombatCrew comparable, Int )
fullPower =
    pilotCheckHelper Crewmate.fullPower CrewmateStatus.fullPower


audaciousGambit : CombatCrew comparable -> { a | currentRound : Int, assignments : Assignments comparable } -> Maybe ( CombatCrew comparable, Int )
audaciousGambit =
    pilotCheckHelper Crewmate.audaciousGambit CrewmateStatus.audaciousGambit


getAcModifier : CombatCrew comparable -> { a | assignments : Assignments comparable } -> Int
getAcModifier crew { assignments } =
    assignments.pilot
        |> Maybe.andThen (\id -> Dict.get id crew)
        |> Maybe.map (Crewmate.getAcModifier << Tuple.first)
        |> Maybe.withDefault 0


getTlModifier : CombatCrew comparable -> { a | assignments : Assignments comparable } -> Int
getTlModifier crew { assignments } =
    assignments.pilot
        |> Maybe.andThen (\id -> Dict.get id crew)
        |> Maybe.map (Crewmate.getTlModifier << Tuple.first)
        |> Maybe.withDefault 0



-- TODO: CrewmateStatus needs to be parameterized to use comparable


demandSource : CombatCrew String -> { a | assignments : Assignments String, target : String, currentRound : Int } -> Maybe (CombatCrew String)
demandSource crew ({ assignments } as r) =
    assignments.captain
        |> Maybe.andThen
            (\id ->
                Dict.get id crew
                    |> Maybe.andThen
                        (\( cm, cms ) ->
                            CrewmateStatus.demandSource cms r
                                |> Maybe.map (\newCms -> Dict.insert id ( cm, newCms ) crew)
                        )
            )


demandTarget : CombatCrew String -> { a | target : String, currentRound : Int } -> Maybe (CombatCrew String)
demandTarget crew ({ target, currentRound } as r) =
    Dict.get target crew
        |> Maybe.map
            (\( cm, cms ) ->
                Dict.insert target ( cm, CrewmateStatus.demandTarget cms r ) crew
            )


encourageSource : CombatCrew String -> { a | assignments : Assignments String, currentRound : Int } -> Maybe (CombatCrew String)
encourageSource crew ({ assignments } as r) =
    assignments.captain
        |> Maybe.andThen
            (\id ->
                Dict.get id crew
                    |> Maybe.andThen
                        (\( cm, cms ) ->
                            CrewmateStatus.encourageSource cms r
                                |> Maybe.map (\newCms -> Dict.insert id ( cm, newCms ) crew)
                        )
            )


encourageTarget : CombatCrew String -> { a | target : String, currentRound : Int } -> Maybe (CombatCrew String)
encourageTarget crew ({ target, currentRound } as r) =
    Dict.get target crew
        |> Maybe.map
            (\( cm, cms ) ->
                Dict.insert target ( cm, CrewmateStatus.encourageTarget cms r ) crew
            )


tauntSource : CombatCrew comparable -> { a | currentRound : Int, assignments : Assignments comparable } -> Maybe ( CombatCrew comparable, ( Int, Int ) )
tauntSource crew ({ assignments } as r) =
    let
        withCaptain captain =
            Dict.get captain crew
                |> Maybe.andThen (withStats captain)

        withStats captain ( cm, cms ) =
            CrewmateStatus.tauntSource cms r
                |> Maybe.map
                    (\( newCms, ( i1, b1 ) ) ->
                        let
                            ( i2, b2 ) =
                                Crewmate.tauntSource cm
                        in
                        ( Dict.insert captain ( cm, newCms ) crew, ( i1 + i2, b1 + b2 ) )
                    )
    in
    assignments.captain
        |> Maybe.andThen withCaptain



-- TODO: This does require a check


ordersSource : CombatCrew comparable -> { a | currentRound : Int, assignments : Assignments comparable } -> Maybe (CombatCrew comparable)
ordersSource crew ({ assignments } as r) =
    let
        withCaptain captain =
            Dict.get captain crew
                |> Maybe.andThen (withStats captain)

        withStats captain ( cm, cms ) =
            Maybe.map2 (\newCm newCms -> ( newCm, newCms ))
                (Crewmate.ordersSource cm)
                (CrewmateStatus.ordersSource cms r)
                |> Maybe.map (\x -> Dict.insert captain x crew)
    in
    assignments.captain
        |> Maybe.andThen withCaptain


ordersTarget : CombatCrew String -> { a | target : String, currentRound : Int } -> Maybe (CombatCrew String)
ordersTarget crew ({ target, currentRound } as r) =
    Dict.get target crew
        |> Maybe.map
            (\( cm, cms ) ->
                Dict.insert target ( cm, CrewmateStatus.ordersTarget cms r ) crew
            )


movingSpeechSource : CombatCrew comparable -> { a | currentRound : Int, assignments : Assignments comparable } -> Maybe ( CombatCrew comparable, Int )
movingSpeechSource crew ({ assignments } as r) =
    let
        withCaptain captain =
            Dict.get captain crew
                |> Maybe.andThen (withStats captain)

        withStats captain ( cm, cms ) =
            Maybe.map2
                (\b1 ( newCms, b2 ) ->
                    ( Dict.insert captain ( cm, newCms ) crew
                    , b1 + b2
                    )
                )
                (Crewmate.movingSpeechSource cm)
                (CrewmateStatus.movingSpeechSource cms r)
    in
    assignments.captain
        |> Maybe.andThen withCaptain
