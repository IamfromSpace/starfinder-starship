module StarshipEditor exposing (..)

import Dict
import Arc exposing (Arc)
import Starship exposing (..)
import Weapon exposing (..)
import Size exposing (..)
import DefenseLevel exposing (..)
import ExpansionBay exposing (..)
import Computer exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (value, disabled, selected)


type alias Model =
    Starship


type Msg
    = SetName String
    | SetPcu Int
    | SetThrusters (ToggleMsg Int)
    | SetArmor (Maybe DefenseLevel)
    | SetComputer (ToggleMsg Computer)
    | SetCrewQuarters CrewQuarters
    | ToggleDefensiveCountermeasures
    | SetDefensiveCountermeasures (Maybe DefenseLevel)
    | SetDriftEngine (Maybe DriftEngine)
    | SetExpansionBay (ListMsg (ToggleMsg ExpansionBay) (Togglable ExpansionBay))
    | SetTurretWeapon (ListMsg (ToggleMsg Weapon) (Togglable Weapon))
    | SetArcWeapon (ArcMsg (ListMsg (ToggleMsg Weapon) (Togglable Weapon)))
    | SetSensors Sensor


mediumTransport : Frame
mediumTransport =
    { name = "Medium Transport"
    , size = Size.Medium
    , maneuverability = Average
    , baseHitPoints = 70
    , hitPointsIncrement = 15
    , damageThreshold = 0
    , criticalThreshold = 14
    , arcMounts =
        { forward = [ Heavy, Light ]
        , aft = [ Light ]
        , portSide = []
        , starboard = []
        }
    , turretMounts = [ Light, Light ]
    , expansionBays = 5
    , minimumCrew = 1
    , maximumCrew = 6
    , listedBuildPoints = 15
    }



-- weapons


coilgun : Weapon
coilgun =
    { name = "Coilgun"
    , range = Long
    , weaponClass = Light
    , weaponType = DirectFire False
    , damage = Just ( 4, 4 )
    , powerDraw = 10
    , buildPoints = 6
    , specialProperties = []
    }


persistentParticleBeam : Weapon
persistentParticleBeam =
    { name = "Persistent Particle Beam"
    , range = Long
    , weaponClass = Heavy
    , weaponType = DirectFire False
    , damage = Just ( 10, 6 )
    , powerDraw = 40
    , buildPoints = 25
    , specialProperties = []
    }


lightPlasmaCannon : Weapon
lightPlasmaCannon =
    { name = "Light Plasma Cannon"
    , range = Short
    , weaponClass = Light
    , weaponType = DirectFire False
    , damage = Just ( 2, 12 )
    , powerDraw = 10
    , buildPoints = 12
    , specialProperties = []
    }


heavyEmpCannon : Weapon
heavyEmpCannon =
    { name = "Heavy EMP Cannon"
    , range = Weapon.Medium
    , weaponClass = Heavy
    , weaponType = DirectFire False
    , damage = Nothing
    , powerDraw = 10
    , buildPoints = 24
    , specialProperties = [ Emp ]
    }


lightLaserCannon : Weapon
lightLaserCannon =
    { name = "Light Laser Cannon"
    , range = Short
    , weaponClass = Light
    , weaponType = DirectFire False
    , damage = Just ( 2, 4 )
    , powerDraw = 5
    , buildPoints = 2
    , specialProperties = []
    }


gyrolaser : Weapon
gyrolaser =
    { name = "Gyrolaser"
    , range = Short
    , weaponClass = Light
    , weaponType = DirectFire False
    , damage = Just ( 1, 8 )
    , powerDraw = 10
    , buildPoints = 3
    , specialProperties = [ BroadArc ]
    }


lightTorpedoLauncher : Weapon
lightTorpedoLauncher =
    { name = "Light Torpedo Launcher"
    , range = Long
    , weaponClass = Light
    , weaponType = Tracking 16
    , damage = Just ( 2, 8 )
    , powerDraw = 5
    , buildPoints = 4
    , specialProperties = []
    }


init : Model
init =
    { name = "Unnamed"
    , frame = mediumTransport
    , powerCoreUnits = 0
    , thrusters = Togglable On 8
    , armor = Nothing
    , computer = Togglable On { bonus = 0, nodes = 0 }
    , crewQuarters = Common
    , defensiveCountermeasures = Nothing
    , driftEngine = Nothing
    , expansionBays = []
    , sensors = { range = Short, bonus = 0 }
    , arcWeapons =
        { forward = []
        , aft = []
        , portSide = []
        , starboard = []
        }
    , turretWeapons = []
    , shields =
        Togglable On
            { name = "Light Shields 10"
            , shieldPoints = 10
            , regenPerMinute = 1
            , powerDraw = 5
            , buildPoints = 2
            }
    }


toggle : Togglable a -> Togglable a
toggle togglable =
    let
        (Togglable switch a) =
            togglable
    in
        case switch of
            On ->
                Togglable Off a

            Off ->
                Togglable On a


setToggled : a -> Togglable a -> Togglable a
setToggled a (Togglable switch _) =
    Togglable switch a


type ToggleMsg a
    = Toggle
    | UpdateToggled a


toggleUpdate : (a -> b -> b) -> ToggleMsg a -> Togglable b -> Togglable b
toggleUpdate innerUpdate toggleMsg togglable =
    case toggleMsg of
        Toggle ->
            toggle togglable

        UpdateToggled innerMsg ->
            let
                (Togglable switch innerModel) =
                    togglable
            in
                Togglable switch (innerUpdate innerMsg innerModel)


type ListMsg a b
    = Cons b
    | UpdateList Int a
    | Delete Int


listUpdate : (a -> b -> b) -> ListMsg a b -> List b -> List b
listUpdate innerUpdate listMsg list =
    case listMsg of
        Cons new ->
            new :: list

        UpdateList index innerMsg ->
            List.indexedMap
                (\i x ->
                    if i == index then
                        innerUpdate innerMsg x
                    else
                        x
                )
                list

        Delete i ->
            List.take i list ++ List.drop (i + 1) list


type ArcMsg a
    = UpdateForward a
    | UpdateAft a
    | UpdatePort a
    | UpdateStarboard a


arcUpdate : (a -> b -> b) -> ArcMsg a -> Arc b -> Arc b
arcUpdate innerUpdate arcMsg arc =
    case arcMsg of
        UpdateForward innerMsg ->
            { arc | forward = innerUpdate innerMsg arc.forward }

        UpdateAft innerMsg ->
            { arc | aft = innerUpdate innerMsg arc.aft }

        UpdatePort innerMsg ->
            { arc | portSide = innerUpdate innerMsg arc.portSide }

        UpdateStarboard innerMsg ->
            { arc | starboard = innerUpdate innerMsg arc.starboard }


update : Msg -> Model -> Model
update action model =
    case action of
        SetName name ->
            { model | name = name }

        SetPcu pcu ->
            { model | powerCoreUnits = pcu }

        SetThrusters toggleMsg ->
            -- We use always here because we don't actually have any messages to send in,
            -- we just want to set the model to the message, and always will do that.
            { model | thrusters = toggleUpdate always toggleMsg model.thrusters }

        SetArmor armor ->
            { model | armor = armor }

        SetComputer toggleMsg ->
            { model | computer = toggleUpdate always toggleMsg model.computer }

        SetCrewQuarters quarters ->
            { model | crewQuarters = quarters }

        ToggleDefensiveCountermeasures ->
            { model | defensiveCountermeasures = Maybe.map toggle model.defensiveCountermeasures }

        SetDefensiveCountermeasures defLevel ->
            { model
                | defensiveCountermeasures =
                    Maybe.map
                        (\dL ->
                            model.defensiveCountermeasures
                                |> Maybe.map (setToggled dL)
                                |> Maybe.withDefault (Togglable On dL)
                        )
                        defLevel
            }

        SetDriftEngine driftEngine ->
            { model | driftEngine = driftEngine }

        SetExpansionBay listMsg ->
            { model
                | expansionBays =
                    listUpdate (toggleUpdate always) listMsg model.expansionBays
            }

        SetTurretWeapon listMsg ->
            { model
                | turretWeapons =
                    listUpdate (toggleUpdate always) listMsg model.turretWeapons
            }

        SetArcWeapon arcMsg ->
            { model
                | arcWeapons =
                    arcUpdate (listUpdate (toggleUpdate always)) arcMsg model.arcWeapons
            }

        SetSensors sensors ->
            { model | sensors = sensors }


weaponView : Weapon -> Html a
weaponView =
    --TODO: Linking
    .name >> text


togglableView : (a -> Html b) -> Togglable a -> Html (ToggleMsg b)
togglableView innerView (Togglable switch inner) =
    div []
        [ Html.map UpdateToggled <| innerView inner
        , text <| " (" ++ toString switch ++ ") "
        , button [ onClick Toggle ] [ text "Toggle" ]
        ]


listView : (a -> Bool) -> Dict.Dict String a -> (a -> Html b) -> List a -> Html (ListMsg b a)
listView canAdd optionMap innerView list =
    let
        items =
            List.indexedMap
                (\i x ->
                    div []
                        [ Html.map (UpdateList i) (innerView x)
                        , button [ onClick (Delete i) ] [ text "Remove" ]
                        ]
                )
                list

        inputCallback str =
            case Dict.get str optionMap of
                Just value ->
                    Cons value

                Nothing ->
                    Debug.crash "Unselectable option was selected!"

        helperOption =
            option
                [ selected True, disabled True ]
                [ text "-- select to add --" ]

        options =
            List.map
                (\( str, x ) ->
                    option [ value str, selected False, disabled (canAdd x) ] [ text str ]
                )
                (Dict.toList optionMap)

        addItem =
            select
                [ onInput inputCallback ]
                (helperOption :: options)
    in
        div [] (addItem :: items)


arcView : (a -> Html b) -> Arc a -> Html (ArcMsg b)
arcView innerView arc =
    div []
        [ div []
            [ text "Forward:"
            , Html.map UpdateForward <| innerView arc.forward
            ]
        , div []
            [ text "Aft:"
            , Html.map UpdateAft <| innerView arc.aft
            ]
        , div []
            [ text "Port:"
            , Html.map UpdatePort <| innerView arc.portSide
            ]
        , div []
            [ text "Starboard:"
            , Html.map UpdateStarboard <| innerView arc.starboard
            ]
        ]


namedToDict : List { a | name : String } -> Dict.Dict String { a | name : String }
namedToDict =
    List.foldr (\x -> Dict.insert x.name x) Dict.empty


weaponDict : Dict.Dict String (Togglable Weapon)
weaponDict =
    (Dict.map (always (Togglable On))
        (namedToDict
            [ coilgun
            , persistentParticleBeam
            , lightPlasmaCannon
            , heavyEmpCannon
            , lightLaserCannon
            , gyrolaser
            , lightTorpedoLauncher
            ]
        )
    )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ label [] [ text "name:" ]
            , input [ onInput SetName, value model.name ] []
            ]
        , div []
            [ div [] [ text <| "Power Core Units: " ++ toString model.powerCoreUnits ]
            , button [ onClick (SetPcu (model.powerCoreUnits + 10)) ] [ text "Increase" ]
            , button [ onClick (SetPcu (model.powerCoreUnits - 10)) ] [ text "Decrease" ]
            ]
        , let
            (Togglable switch speed) =
                model.thrusters
          in
            Html.map SetThrusters <|
                div []
                    [ div [] [ text <| "Thrusters (" ++ toString switch ++ "): " ++ toString speed ]
                    , button [ onClick Toggle ] [ text "Toggle Status" ]
                    , button
                        [ disabled (topSpeed model.frame.size < speed + 1)
                        , onClick (UpdateToggled (speed + 1))
                        ]
                        [ text "Increase" ]
                    , button
                        [ disabled (speed <= 1)
                        , onClick (UpdateToggled (speed - 1))
                        ]
                        [ text "Decrease" ]
                    ]
        , div [] <|
            case model.armor of
                Just dL ->
                    [ div [] [ text <| "Armor: " ++ toString dL ]
                    , button
                        [ disabled (incDefenseLevel dL == Nothing)
                        , onClick (SetArmor (incDefenseLevel dL))
                        ]
                        [ text "Increase" ]
                    , button [ onClick (SetArmor (decDefenseLevel dL)) ] [ text "Decrease" ]
                    ]

                Nothing ->
                    [ div [] [ text <| "Armor: None" ]
                    , button [ onClick (SetArmor (Just Mk1)) ] [ text "Add Armor" ]
                    ]
        , let
            (Togglable switch computer) =
                model.computer

            nodeText =
                computer.bonus
                    |> List.repeat computer.nodes
                    |> List.map (toString >> (++) "+")
                    |> String.join "/"
          in
            Html.map SetComputer <|
                div []
                    (if computer.nodes > 0 && computer.bonus > 0 then
                        [ div []
                            [ text <| "Computer (" ++ toString switch ++ "): " ++ nodeText ]
                        , button [ onClick Toggle ] [ text "Toggle Status" ]
                        , button
                            [ onClick (UpdateToggled { computer | nodes = computer.nodes - 1 })
                            ]
                            [ text "Remove Node" ]
                        , button
                            [ onClick (UpdateToggled { computer | nodes = computer.nodes + 1 }) ]
                            [ text "Add Node" ]
                        , button
                            [ onClick (UpdateToggled { computer | bonus = computer.bonus - 1 })
                            ]
                            [ text "Decrease Bonus" ]
                        , button
                            [ onClick (UpdateToggled { computer | bonus = computer.bonus + 1 }) ]
                            [ text "Increase Bonus" ]
                        ]
                     else
                        [ div [] [ text "Computer: None" ]
                        , button
                            [ onClick (UpdateToggled { nodes = 1, bonus = 1 }) ]
                            [ text "Add Computer" ]
                        ]
                    )
        , let
            mkOption x =
                option [ value (toString x), (selected (model.crewQuarters == x)) ]
                    [ text (toString x) ]

            inputCallback str =
                SetCrewQuarters <|
                    case str of
                        "GoodQuarters" ->
                            GoodQuarters

                        "Luxurious" ->
                            Luxurious

                        _ ->
                            Common
          in
            div []
                [ select [ onInput inputCallback ]
                    [ mkOption Common
                    , mkOption GoodQuarters
                    , mkOption Luxurious
                    ]
                ]
        , div [] <|
            case model.defensiveCountermeasures of
                Just (Togglable switch dL) ->
                    [ div [] [ text <| "Defensive Countermeasures (" ++ toString switch ++ "): " ++ toString dL ]
                    , button [ onClick (ToggleDefensiveCountermeasures) ] [ text "Toggle Status" ]
                    , button
                        [ disabled (incDefenseLevel dL == Nothing)
                        , onClick (SetDefensiveCountermeasures (incDefenseLevel dL))
                        ]
                        [ text "Increase" ]
                    , button [ onClick (SetDefensiveCountermeasures (decDefenseLevel dL)) ] [ text "Decrease" ]
                    ]

                Nothing ->
                    [ div [] [ text <| "Defensive Countermeasures: None" ]
                    , button [ onClick (SetDefensiveCountermeasures (Just Mk1)) ] [ text "Add Defensive Countermeasures" ]
                    ]
        , case model.driftEngine of
            Just driftEngine ->
                let
                    mkOption x =
                        option [ value (toString x), (selected (driftEngine == x)) ]
                            [ text (toString x) ]

                    inputCallback str =
                        SetDriftEngine <|
                            Just <|
                                case str of
                                    "Booster" ->
                                        Booster

                                    "Major" ->
                                        Major

                                    "Superior" ->
                                        Superior

                                    "Ultra" ->
                                        Ultra

                                    _ ->
                                        Basic
                in
                    div []
                        [ div [] [ text <| "Drift Engine: " ++ toString driftEngine ]
                        , select [ onInput inputCallback ]
                            [ mkOption Basic
                            , mkOption Booster
                            , mkOption Major
                            , mkOption Superior
                            , mkOption Ultra
                            ]
                        , button
                            [ onClick (SetDriftEngine Nothing) ]
                            [ text "Remove Drift Engine" ]
                        ]

            Nothing ->
                div []
                    [ div [] [ text "Drift Engine: None" ]
                    , button
                        [ onClick (SetDriftEngine (Just Booster)) ]
                        [ text "Add Drift Engine" ]
                    ]
        , let
            mkOption x =
                let
                    baysUsedSoFar =
                        model.expansionBays
                            |> List.map
                                ((\(Togglable _ x) -> x) >> ExpansionBay.getExpansionBaysUsed)
                            |> List.sum

                    notEnoughSlots =
                        getExpansionBaysUsed x + baysUsedSoFar > model.frame.expansionBays

                    fitsOnFrame =
                        isValidSize model.frame.size x
                in
                    option
                        [ value (toString x)
                        , disabled (notEnoughSlots || not fitsOnFrame)
                        ]
                        [ text (toString x) ]

            inputCallback str =
                Cons <|
                    Togglable On <|
                        case str of
                            "CargoHold" ->
                                CargoHold

                            "EscapePods" ->
                                EscapePods

                            "GuestQuarters" ->
                                GuestQuarters

                            "HangarBay" ->
                                HangarBay

                            "LifeBoats" ->
                                LifeBoats

                            "MedicalBay" ->
                                MedicalBay

                            "PassengerSeating" ->
                                PassengerSeating

                            "PowerCoreHousing" ->
                                PowerCoreHousing

                            "RecreationSuiteGym" ->
                                RecreationSuiteGym

                            "RecreationSuiteTrivedDen" ->
                                RecreationSuiteTrivedDen

                            "RecreationSuiteHac" ->
                                RecreationSuiteHac

                            "ScienceLab" ->
                                ScienceLab

                            "SealedEnvironmentChamber" ->
                                SealedEnvironmentChamber

                            "ShuttleBay" ->
                                ShuttleBay

                            "SmugglerCompartment 20" ->
                                SmugglerCompartment 20

                            "SynthesisBay" ->
                                SynthesisBay

                            "TechWorkshop" ->
                                TechWorkshop

                            _ ->
                                ArcaneLaboratory

            expansionBayView switch bay =
                div [] <|
                    case bay of
                        SmugglerCompartment dc ->
                            [ div [] [ text <| "SmugglerCompartment (" ++ toString switch ++ "): " ++ toString dc ]
                              -- TODO: Currently there are no validations on min/max DC (20-50)
                            , button [ onClick <| UpdateToggled <| SmugglerCompartment <| dc + 5 ] [ text "Increase" ]
                            , button [ onClick <| UpdateToggled <| SmugglerCompartment <| dc - 5 ] [ text "Decrease" ]
                            , button [ onClick Toggle ] [ text "Toggle" ]
                            ]

                        b ->
                            [ div [] [ text <| toString b ++ " (" ++ toString switch ++ ")" ]
                            , button [ onClick Toggle ] [ text "Toggle" ]
                            ]
          in
            Html.map SetExpansionBay <|
                div []
                    [ div [] [ text "ExpansionBays:" ]
                    , div []
                        (List.indexedMap
                            (\index (Togglable switch bay) ->
                                div []
                                    [ Html.map (UpdateList index) <| expansionBayView switch bay
                                    , button [ onClick <| Delete index ] [ text "Remove" ]
                                    ]
                            )
                            model.expansionBays
                        )
                    , label [] [ text "Add expansion bay:" ]
                    , select [ onInput inputCallback, value "" ]
                        [ option
                            [ selected True, disabled True ]
                            [ text "-- select an expansion bay to add --" ]
                        , mkOption ArcaneLaboratory
                        , mkOption CargoHold
                        , mkOption EscapePods
                        , mkOption GuestQuarters
                        , mkOption HangarBay
                        , mkOption LifeBoats
                        , mkOption MedicalBay
                        , mkOption PassengerSeating
                        , mkOption PowerCoreHousing
                        , mkOption RecreationSuiteGym
                        , mkOption RecreationSuiteTrivedDen
                        , mkOption RecreationSuiteHac
                        , mkOption ScienceLab
                        , mkOption SealedEnvironmentChamber
                        , mkOption ShuttleBay
                        , mkOption (SmugglerCompartment 20)
                        , mkOption SynthesisBay
                        , mkOption TechWorkshop
                        ]
                    ]
        , let
            sensors =
                model.sensors

            mkOption x =
                option [ value (toString x), (selected (sensors.range == x)) ]
                    [ text (toString x) ]

            -- TODO: This is really a general util function
            formatBonus x =
                (if x < 0 then
                    ""
                 else
                    "+"
                )
                    ++ toString x

            inputCallback str =
                SetSensors <|
                    { sensors
                        | range =
                            case str of
                                "Long" ->
                                    Long

                                "Medium" ->
                                    Weapon.Medium

                                _ ->
                                    Short
                    }
          in
            div []
                [ div []
                    [ text <| "Sensors (" ++ formatBonus sensors.bonus ++ "):" ]
                , select [ onInput inputCallback ]
                    [ mkOption Short
                    , mkOption Weapon.Medium
                    , mkOption Long
                    ]
                , button
                    [ onClick (SetSensors { sensors | bonus = sensors.bonus + 1 }) ]
                    [ text "Increase Bonus" ]
                , button
                    [ onClick (SetSensors { sensors | bonus = sensors.bonus - 1 }) ]
                    [ text "Decrease Bonus" ]
                ]
        , Html.map SetTurretWeapon <|
            div []
                [ div [] [ text "Turret Weapons:" ]
                , listView
                    (\(Togglable _ weapon) ->
                        not (List.member weapon.weaponClass (getAllowedClasses model.frame.size))
                            || (weapon.weaponClass == Capital)
                    )
                    weaponDict
                    (togglableView weaponView)
                    model.turretWeapons
                ]
        , Html.map SetArcWeapon <|
            div []
                [ div [] [ text "Arc Weapons:" ]
                , arcView
                    (listView
                        (\(Togglable _ weapon) ->
                            not (List.member weapon.weaponClass (getAllowedClasses model.frame.size))
                        )
                        weaponDict
                        (togglableView weaponView)
                    )
                    model.arcWeapons
                ]
        , div [] [ text <| "Total Power Draw: " ++ toString (getStarshipPowerDraw model) ++ " PCU" ]
        , div [] [ text <| "Total Build Points: " ++ toString (getStarshipBuildPoints model) ]
        , div [] [ text <| "Tier: " ++ toString (getTierFromBuildPoints (getStarshipBuildPoints model)) ]
        , div []
            (case validateStarship model of
                [] ->
                    [ text "Starship is valid" ]

                x ->
                    div [] [ text "Starship has errors!:" ]
                        :: List.map (\err -> div [] [ text (" - " ++ toString err) ]) x
            )
        ]


main : Program Never Model Msg
main =
    beginnerProgram { model = init, update = update, view = view }
