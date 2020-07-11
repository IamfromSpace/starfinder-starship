module StarshipEditor exposing (ArcMsg(..), LinkAndToggleMsg(..), ListMsg(..), Model, Msg(..), ToggleMsg(..), arcUpdate, arcView, init, linkAndTogglableView, linkAndToggleUpdate, listUpdate, listView, main, namedToDict, selectionView, togglableView, toggleUpdate, update, view, weaponDict, weaponView)

import Arc exposing (Arc)
import Browser exposing (sandbox)
import Computer exposing (..)
import DefenseLevel exposing (..)
import Dict
import ExpansionBay exposing (..)
import Html exposing (..)
import Html.Attributes exposing (disabled, selected, value)
import Html.Events exposing (onClick, onInput)
import KeyedSet as KS
import Link exposing (..)
import LinkAndTogglable as LT exposing (LinkAndTogglable)
import ShipAssets exposing (..)
import Size exposing (..)
import Starship exposing (..)
import Switch
import Togglable exposing (..)
import Weapon exposing (..)


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
    | SetTurretWeapon (ListMsg (LinkAndToggleMsg Weapon) (LinkAndTogglable Weapon))
    | SetArcWeapon (ArcMsg (ListMsg (LinkAndToggleMsg Weapon) (LinkAndTogglable Weapon)))
    | SetShields (ToggleMsg Shields)
    | SetSensors Sensor


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
    , shields =
        -- TODO: better way than this??
        pure <|
            case KS.get "Basic Shields 10" shields of
                Just s ->
                    s

                Nothing ->
                    Debug.todo "Missing Basic Shields 10 definition"
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


type LinkAndToggleMsg a
    = LTToggle
    | Link
    | Unlink
    | UpdateInner a


linkAndToggleUpdate :
    (a -> b -> b)
    -> LinkAndToggleMsg a
    -> LinkAndTogglable b
    -> LinkAndTogglable b
linkAndToggleUpdate innerUpdate toggleMsg togglable =
    case toggleMsg of
        LTToggle ->
            LT.toggle togglable

        Link ->
            LT.link togglable

        Unlink ->
            LT.unlink togglable

        UpdateInner innerMsg ->
            LT.map (innerUpdate innerMsg) togglable


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
                    listUpdate (linkAndToggleUpdate always) listMsg model.turretWeapons
            }

        SetArcWeapon arcMsg ->
            { model
                | arcWeapons =
                    arcUpdate (listUpdate (linkAndToggleUpdate always)) arcMsg model.arcWeapons
            }

        SetShields toggleMsg ->
            { model | shields = toggleUpdate always toggleMsg model.shields }

        SetSensors sensors ->
            { model | sensors = sensors }


weaponView : Weapon -> Html a
weaponView =
    .name >> text


togglableView : (a -> Html b) -> Togglable a -> Html (ToggleMsg b)
togglableView innerView togglable =
    div []
        [ Html.map UpdateToggled <| innerView (extract togglable)
        , text <| " (" ++ Switch.toString (meta togglable) ++ ") "
        , button [ onClick Toggle ] [ text "Toggle" ]
        ]


linkAndTogglableView : (a -> Bool) -> (a -> Html b) -> LinkAndTogglable a -> Html (LinkAndToggleMsg b)
linkAndTogglableView testCanLink innerView togglable =
    let
        m =
            LT.meta togglable

        canLink =
            testCanLink (LT.extract togglable)

        linkText =
            if canLink then
                ", " ++ Link.toString (.link m)

            else
                ""

        linkButton =
            case .link (LT.meta togglable) of
                Linked ->
                    button [ onClick Unlink ] [ text "Unlink" ]

                Unlinked ->
                    button [ onClick Link ] [ text "Link" ]

        base =
            [ Html.map UpdateInner <| innerView (LT.extract togglable)
            , text <| " (" ++ Switch.toString (.switch m) ++ linkText ++ ") "
            , button [ onClick LTToggle ] [ text "Toggle" ]
            ]
    in
    div []
        (if canLink then
            base ++ [ linkButton ]

         else
            base
        )


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
                    Debug.todo "Unselectable option was selected!"

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
                        Debug.todo "Unselectable option was selected!"
            )
        ]
        (Dict.values <|
            Dict.map
                (\str y ->
                    option [ value str, selected (y == x), disabled (not (canAdd x)) ] [ text str ]
                )
                optionMap
        )


weaponDict : Dict.Dict String (LinkAndTogglable Weapon)
weaponDict =
    Dict.map (always LT.pure)
        (KS.toDict weapons)


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
                    (KS.toDict frames)
                    model.frame
                ]
        , div []
            [ div [] [ text <| "Power Core Units: " ++ String.fromInt model.powerCoreUnits ]
            , button [ onClick (SetPcu (model.powerCoreUnits + 10)) ] [ text "Increase" ]
            , button [ onClick (SetPcu (model.powerCoreUnits - 10)) ] [ text "Decrease" ]
            ]
        , let
            speed =
                extract model.thrusters
          in
          Html.map SetThrusters <|
            div []
                [ div [] [ text <| "Thrusters (" ++ Switch.toString (meta model.thrusters) ++ "): " ++ String.fromInt speed ]
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
                    [ div [] [ text <| "Armor: " ++ DefenseLevel.toString dL ]
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
                    |> List.map (String.fromInt >> (++) "+")
                    |> String.join "/"
          in
          Html.map SetComputer <|
            div []
                (if computer.nodes > 0 && computer.bonus > 0 then
                    [ div []
                        [ text <| "Computer (" ++ Switch.toString (meta model.computer) ++ "): " ++ nodeText ]
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
                option [ value (crewQuartersToString x), selected (model.crewQuarters == x) ]
                    [ text (crewQuartersToString x) ]

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
                    [ div [] [ text <| "Defensive Countermeasures (" ++ Switch.toString (meta togglable) ++ "): " ++ DefenseLevel.toString dL ]
                    , button [ onClick ToggleDefensiveCountermeasures ] [ text "Toggle Status" ]
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
                        option [ value (driftEngineToString x), selected (driftEngine == x) ]
                            [ text (driftEngineToString x) ]

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
                    [ div [] [ text <| "Drift Engine: " ++ driftEngineToString driftEngine ]
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
                            [ div [] [ text <| "SmugglerCompartment: " ++ String.fromInt dc ]

                            -- TODO: Currently there are no validations on min/max DC (20-50)
                            , button [ onClick <| SmugglerCompartment <| dc + 5 ] [ text "Increase" ]
                            , button [ onClick <| SmugglerCompartment <| dc - 5 ] [ text "Decrease" ]
                            ]

                        b ->
                            [ div [] [ text <| ExpansionBay.toString b ] ]
          in
          Html.map SetExpansionBay <|
            div []
                [ div [] [ text "ExpansionBays:" ]
                , listView
                    (extract >> canAdd)
                    (List.foldr (\b -> Dict.insert (ExpansionBay.toString b) (pure b)) Dict.empty bays)
                    (togglableView expansionBayView)
                    model.expansionBays
                ]
        , let
            sensors =
                model.sensors

            mkOption x =
                option [ value (rangeToString x), selected (sensors.range == x) ]
                    [ text (rangeToString x) ]

            -- TODO: This is really a general util function
            formatBonus x =
                (if x < 0 then
                    ""

                 else
                    "+"
                )
                    ++ String.fromInt x

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
                    (LT.extract
                        >> (\weapon ->
                                not (List.member weapon.weaponClass (getAllowedClasses model.frame.size))
                                    || (weapon.weaponClass == Capital)
                           )
                    )
                    weaponDict
                    (linkAndTogglableView
                        (isTrackingWeapon >> not)
                        weaponView
                    )
                    model.turretWeapons
                ]
        , Html.map SetArcWeapon <|
            div []
                [ div [] [ text "Arc Weapons:" ]
                , arcView
                    (listView
                        (LT.extract
                            >> (\weapon ->
                                    not (List.member weapon.weaponClass (getAllowedClasses model.frame.size))
                               )
                        )
                        weaponDict
                        (linkAndTogglableView
                            (isTrackingWeapon >> not)
                            weaponView
                        )
                    )
                    model.arcWeapons
                ]
        , Html.map SetShields <|
            div []
                [ div [] [ text "Shields:" ]
                , togglableView
                    (selectionView
                        (always True)
                        (KS.toDict shields)
                    )
                    model.shields
                ]
        , div [] [ text <| "Total Power Draw: " ++ String.fromInt (getStarshipPowerDraw model) ++ " PCU" ]
        , div [] [ text <| "Total Build Points: " ++ String.fromInt (getStarshipBuildPoints model) ]
        , div [] [ text <| "Tier: " ++ String.fromFloat (getTierFromBuildPoints (getStarshipBuildPoints model)) ]
        , div []
            (case validateStarship model of
                [] ->
                    [ text "Starship is valid" ]

                x ->
                    div [] [ text "Starship has errors!:" ]
                        :: List.map (\err -> div [] [ text (" - " ++ buildErrorToString err) ]) x
            )
        ]


main : Program () Model Msg
main =
    sandbox { init = init, update = update, view = view }
