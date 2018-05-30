module StarshipEditor exposing (..)

import Dict
import Arc exposing (Arc)
import Starship exposing (..)
import Togglable exposing (..)
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
    | SetFrame Frame
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
    | SetShields (ToggleMsg Shields)
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


fighter : Frame
fighter =
    { name = "Fighter"
    , size = Size.Tiny
    , maneuverability = Good
    , baseHitPoints = 35
    , hitPointsIncrement = 5
    , damageThreshold = 0
    , criticalThreshold = 7
    , arcMounts =
        { forward = [ Light, Light ]
        , aft = [ Light ]
        , portSide = []
        , starboard = []
        }
    , turretMounts = []
    , expansionBays = 0
    , minimumCrew = 1
    , maximumCrew = 2
    , listedBuildPoints = 8
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



--shields


lightShields60 : Shields
lightShields60 =
    { name = "Light Shields 60"
    , shieldPoints = 60
    , regenPerMinute = 2
    , powerDraw = 20
    , buildPoints = 10
    }


lightShields80 : Shields
lightShields80 =
    { name = "Light Shields 80"
    , shieldPoints = 80
    , regenPerMinute = 2
    , powerDraw = 30
    , buildPoints = 12
    }


lightShields10 : Shields
lightShields10 =
    { name = "Light Shields 10"
    , shieldPoints = 10
    , regenPerMinute = 1
    , powerDraw = 5
    , buildPoints = 2
    }


init : Model
init =
    { name = "Unnamed"
    , frame = mediumTransport
    , powerCoreUnits = 0
    , thrusters = pure 8
    , armor = Nothing
    , computer = pure { bonus = 0, nodes = 0 }
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
    , shields = pure lightShields10
    }


type ToggleMsg a
    = Toggle
    | UpdateToggled a


toggleUpdate : (a -> b -> b) -> ToggleMsg a -> Togglable b -> Togglable b
toggleUpdate innerUpdate toggleMsg togglable =
    case toggleMsg of
        Toggle ->
            toggle togglable

        UpdateToggled innerMsg ->
            Togglable.map (innerUpdate innerMsg) togglable


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

        SetFrame frame ->
            { model | frame = frame }

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
                                |> Maybe.map (Togglable.map (always dL))
                                |> Maybe.withDefault (pure dL)
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

        SetShields toggleMsg ->
            { model | shields = toggleUpdate always toggleMsg model.shields }

        SetSensors sensors ->
            { model | sensors = sensors }


weaponView : Weapon -> Html a
weaponView =
    --TODO: Linking
    .name >> text


togglableView : (a -> Html b) -> Togglable a -> Html (ToggleMsg b)
togglableView innerView togglable =
    div []
        [ Html.map UpdateToggled <| innerView (extract togglable)
        , text <| " (" ++ toString (meta togglable) ++ ") "
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


selectionView : (a -> Bool) -> Dict.Dict String a -> a -> Html a
selectionView canAdd optionMap x =
    select
        [ onInput
            (\str ->
                case Dict.get str optionMap of
                    Just v ->
                        v

                    Nothing ->
                        Debug.crash "Unselectable option was selected!"
            )
        ]
        (Dict.values <|
            Dict.map
                (\str y ->
                    option [ value str, selected (y == x), disabled (not (canAdd x)) ] [ text str ]
                )
                optionMap
        )


weaponDict : Dict.Dict String (Togglable Weapon)
weaponDict =
    (Dict.map (always pure)
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
        , Html.map SetFrame <|
            div []
                [ div [] [ text "Frame:" ]
                , selectionView
                    (always True)
                    (namedToDict [ mediumTransport, fighter ])
                    model.frame
                ]
        , div []
            [ div [] [ text <| "Power Core Units: " ++ toString model.powerCoreUnits ]
            , button [ onClick (SetPcu (model.powerCoreUnits + 10)) ] [ text "Increase" ]
            , button [ onClick (SetPcu (model.powerCoreUnits - 10)) ] [ text "Decrease" ]
            ]
        , let
            speed =
                extract model.thrusters
          in
            Html.map SetThrusters <|
                div []
                    [ div [] [ text <| "Thrusters (" ++ toString (meta model.thrusters) ++ "): " ++ toString speed ]
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
            computer =
                extract model.computer

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
                            [ text <| "Computer (" ++ toString (meta model.computer) ++ "): " ++ nodeText ]
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
                Just togglable ->
                    let
                        dL =
                            extract togglable
                    in
                        [ div [] [ text <| "Defensive Countermeasures (" ++ toString (meta togglable) ++ "): " ++ toString dL ]
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
            canAdd x =
                let
                    baysUsedSoFar =
                        model.expansionBays
                            |> List.map
                                (extract >> ExpansionBay.getExpansionBaysUsed)
                            |> List.sum

                    notEnoughSlots =
                        getExpansionBaysUsed x + baysUsedSoFar > model.frame.expansionBays

                    fitsOnFrame =
                        isValidSize model.frame.size x
                in
                    notEnoughSlots || not fitsOnFrame

            bays =
                [ ArcaneLaboratory
                , CargoHold
                , EscapePods
                , GuestQuarters
                , HangarBay
                , LifeBoats
                , MedicalBay
                , PassengerSeating
                , PowerCoreHousing
                , RecreationSuiteGym
                , RecreationSuiteTrivedDen
                , RecreationSuiteHac
                , ScienceLab
                , SealedEnvironmentChamber
                , ShuttleBay
                , SmugglerCompartment 20
                , SynthesisBay
                , TechWorkshop
                ]

            expansionBayView bay =
                div [] <|
                    case bay of
                        SmugglerCompartment dc ->
                            [ div [] [ text <| "SmugglerCompartment: " ++ toString dc ]
                              -- TODO: Currently there are no validations on min/max DC (20-50)
                            , button [ onClick <| SmugglerCompartment <| dc + 5 ] [ text "Increase" ]
                            , button [ onClick <| SmugglerCompartment <| dc - 5 ] [ text "Decrease" ]
                            ]

                        b ->
                            [ div [] [ text <| toString b ] ]
          in
            Html.map SetExpansionBay <|
                div []
                    [ div [] [ text "ExpansionBays:" ]
                    , listView
                        (extract >> canAdd)
                        (List.foldr (\b -> Dict.insert (toString b) (pure b)) Dict.empty bays)
                        (togglableView expansionBayView)
                        model.expansionBays
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
                    (extract
                        >> (\weapon ->
                                not (List.member weapon.weaponClass (getAllowedClasses model.frame.size))
                                    || (weapon.weaponClass == Capital)
                           )
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
                        (extract
                            >> (\weapon ->
                                    not (List.member weapon.weaponClass (getAllowedClasses model.frame.size))
                               )
                        )
                        weaponDict
                        (togglableView weaponView)
                    )
                    model.arcWeapons
                ]
        , Html.map SetShields <|
            div []
                [ div [] [ text "Shields:" ]
                , togglableView
                    (selectionView
                        (always True)
                        (namedToDict [ lightShields60, lightShields10, lightShields80 ])
                    )
                    model.shields
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
