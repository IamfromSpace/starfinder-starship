module Tests exposing (..)

import Test exposing (..)
import Expect
import Starship exposing (..)
import Weapon exposing (..)
import ExpansionBay exposing (..)


-- TODO: These things should probably live in an assets directory that
-- serves both test and the application
-- frames


mediumTransport : Frame
mediumTransport =
    { name = "Medium Transport"
    , size = Starship.Medium
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



-- ships


norikamaDropship : Starship
norikamaDropship =
    { name = "Norkama Dropship"
    , frame = mediumTransport
    , powerCoreUnits = 250
    , thrusters = Togglable True 12
    , armor = Just Mk6
    , computer = Togglable True { bonus = 3, nodes = 2 }
    , crewQuarters = Common
    , defensiveCountermeasures = Just (Togglable True Mk6)
    , driftEngine = Booster
    , expansionBays = [ Togglable True GuestQuarters ]
    , sensors = { range = Long, bonus = 2 }
    , arcWeapons =
        { forward = [ Togglable True coilgun, Togglable True persistentParticleBeam ]
        , aft = [ Togglable True coilgun ]
        , portSide = []
        , starboard = []
        }
    , turretWeapons = [ Togglable True lightPlasmaCannon, Togglable True lightPlasmaCannon ]
    , shields =
        Togglable True
            { name = "Light Shields 80"
            , shieldPoints = 80
            , regenPerMinute = 2
            , powerDraw = 30
            , buildPoints = 12
            }
    }


blackwindSepulcher : Starship
blackwindSepulcher =
    { name = "Blackwind Sepulcher"
    , frame = mediumTransport
    , powerCoreUnits = 140
    , thrusters = Togglable True 8
    , armor = Just Mk5
    , computer = Togglable True { bonus = 0, nodes = 0 }
    , crewQuarters = Common
    , defensiveCountermeasures = Just (Togglable True Mk6)
    , driftEngine = Booster
    , expansionBays =
        [ Togglable True CargoHold
        , Togglable True CargoHold
        , Togglable True CargoHold
        , Togglable True CargoHold
        , Togglable True CargoHold
        ]
    , sensors = { range = Long, bonus = 2 }
    , arcWeapons =
        { forward = [ Togglable True heavyEmpCannon, Togglable True lightLaserCannon ]
        , aft = [ Togglable True gyrolaser ]
        , portSide = []
        , starboard = []
        }
    , turretWeapons = [ Togglable True lightTorpedoLauncher, Togglable True lightTorpedoLauncher ]
    , shields =
        Togglable True
            { name = "Light Shields 60"
            , shieldPoints = 60
            , regenPerMinute = 2
            , powerDraw = 20
            , buildPoints = 10
            }
    }


all : Test
all =
    describe "Starship"
        [ describe "Build Points"
            [ test "Norikama Dropship" <|
                \() ->
                    Expect.equal 201 (getStarshipBuildPoints norikamaDropship)
            , test "Blackwind Sepulcher" <|
                \() ->
                    Expect.equal 137 (getStarshipBuildPoints blackwindSepulcher)
            ]
        , describe "Build Validation"
            [ test "Norikama Dropship" <|
                \() ->
                    Expect.equal [] (validateStarship norikamaDropship)
            , test "Blackwind Sepulcher" <|
                \() ->
                    Expect.equal [] (validateStarship blackwindSepulcher)
            ]
        , describe "Max Hit Points"
            [ test "Norikama Dropship" <|
                \() ->
                    Expect.equal 100 (getMaxHitPoints norikamaDropship)
            , test "Blackwind Sepulcher" <|
                \() ->
                    Expect.equal 85 (getMaxHitPoints blackwindSepulcher)
            ]
        ]
