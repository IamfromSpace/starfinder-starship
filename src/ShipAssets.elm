module ShipAssets exposing (..)

import Starship exposing (..)
import Weapon exposing (..)
import Size exposing (..)
import Starship exposing (..)
import Weapon exposing (..)
import ExpansionBay exposing (..)
import DefenseLevel exposing (..)
import Togglable exposing (..)
import LinkAndTogglable as LT exposing (LinkAndTogglable)


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
    , weaponType = DirectFire
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
    , weaponType = DirectFire
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
    , weaponType = DirectFire
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
    , weaponType = DirectFire
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
    , weaponType = DirectFire
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
    , weaponType = DirectFire
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


norikamaDropship : Starship
norikamaDropship =
    { name = "Norkama Dropship"
    , frame = mediumTransport
    , powerCoreUnits = 250
    , thrusters = pure 12
    , armor = Just Mk6
    , computer = pure { bonus = 3, nodes = 2 }
    , crewQuarters = Common
    , defensiveCountermeasures = Just (pure Mk6)
    , driftEngine = Just Booster
    , expansionBays = [ pure GuestQuarters ]
    , sensors = { range = Long, bonus = 2 }
    , arcWeapons =
        { forward = [ LT.pure coilgun, LT.pure persistentParticleBeam ]
        , aft = [ LT.pure coilgun ]
        , portSide = []
        , starboard = []
        }
    , turretWeapons = [ LT.pure lightPlasmaCannon, LT.pure lightPlasmaCannon ]
    , shields =
        pure
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
    , thrusters = pure 8
    , armor = Just Mk5
    , computer = pure { bonus = 0, nodes = 0 }
    , crewQuarters = Common
    , defensiveCountermeasures = Just (pure Mk6)
    , driftEngine = Just Booster
    , expansionBays =
        [ pure CargoHold
        , pure CargoHold
        , pure CargoHold
        , pure CargoHold
        , pure CargoHold
        ]
    , sensors = { range = Long, bonus = 2 }
    , arcWeapons =
        { forward = [ LT.pure heavyEmpCannon, LT.pure lightLaserCannon ]
        , aft = [ LT.pure gyrolaser ]
        , portSide = []
        , starboard = []
        }
    , turretWeapons = [ LT.pure lightTorpedoLauncher, LT.pure lightTorpedoLauncher ]
    , shields =
        pure
            { name = "Light Shields 60"
            , shieldPoints = 60
            , regenPerMinute = 2
            , powerDraw = 20
            , buildPoints = 10
            }
    }
