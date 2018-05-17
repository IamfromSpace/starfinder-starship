module StarshipEditor exposing (..)

import Starship exposing (..)
import Weapon exposing (..)
import Size exposing (..)
import DefenseLevel exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (value, disabled)


type alias Model =
    Starship


type Msg
    = SetName String
    | SetPcu Int
    | ToggleThrusters
    | SetSpeed Int
    | SetArmor (Maybe DefenseLevel)


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


update : Msg -> Model -> Model
update action model =
    case action of
        SetName name ->
            { model | name = name }

        SetPcu pcu ->
            { model | powerCoreUnits = pcu }

        ToggleThrusters ->
            let
                (Togglable switch speed) =
                    model.thrusters
            in
                case switch of
                    On ->
                        { model | thrusters = Togglable Off speed }

                    Off ->
                        { model | thrusters = Togglable On speed }

        SetSpeed speed ->
            let
                (Togglable switch _) =
                    model.thrusters
            in
                { model | thrusters = Togglable switch speed }

        SetArmor armor ->
            { model | armor = armor }


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
            div []
                [ div [] [ text <| "Thrusters (" ++ toString switch ++ "): " ++ toString speed ]
                , button [ onClick (ToggleThrusters) ] [ text "Toggle Status" ]
                , button
                    [ disabled (topSpeed model.frame.size < speed + 1)
                    , onClick (SetSpeed (speed + 1))
                    ]
                    [ text "Increase" ]
                , button
                    [ disabled (speed <= 1)
                    , onClick (SetSpeed (speed - 1))
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
        , div [] [ text <| "Total Power Draw: " ++ toString (getStarshipPowerDraw model) ++ " PCU" ]
        , div [] [ text <| "Total Build Points: " ++ toString (getStarshipBuildPoints model) ]
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
