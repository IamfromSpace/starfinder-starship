module ShipAssets exposing (blackwindSepulcher, frames, mediumTransport, norikamaDropship, shields, weapons)

import DefenseLevel exposing (..)
import ExpansionBay exposing (..)
import KeyedSet as KS exposing (KeyedSet)
import LinkAndTogglable as LT exposing (LinkAndTogglable)
import Size exposing (..)
import Starship exposing (..)
import Togglable exposing (..)
import Weapon exposing (..)


mediumTransport : Frame
mediumTransport =
    -- TODO: Delete this and the full ship definitions
    { name = "Medium Transport"
    , size = Size.Medium
    , maneuverability = Average
    , baseHitPoints = 70
    , hitPointsIncrement = 15
    , damageThreshold = 0
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


frames : KeyedSet String Frame
frames =
    KS.fromList
        .name
        [ { name = "Medium Transport"
          , size = Size.Medium
          , maneuverability = Average
          , baseHitPoints = 70
          , hitPointsIncrement = 15
          , damageThreshold = 0
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
        , { name = "Medium Explorer"
          , size = Size.Medium
          , maneuverability = Good
          , baseHitPoints = 55
          , hitPointsIncrement = 10
          , damageThreshold = 0
          , arcMounts =
                { forward = [ Light ]
                , aft = []
                , portSide = [ Light ]
                , starboard = [ Light ]
                }
          , turretMounts = [ Light ]
          , expansionBays = 4
          , minimumCrew = 1
          , maximumCrew = 6
          , listedBuildPoints = 12
          }
        ]



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


weapons : KS.KeyedSet String Weapon
weapons =
    KS.fromList
        .name
        -- Light Direct
        [ { name = "Chain Cannon"
          , range = Short
          , weaponClass = Light
          , weaponType = DirectFire
          , damage = Just ( 6, 4 )
          , powerDraw = 15
          , buildPoints = 15
          , specialProperties = [ Ripper ]
          }
        , { name = "Coilgun"
          , range = Long
          , weaponClass = Light
          , weaponType = DirectFire
          , damage = Just ( 4, 4 )
          , powerDraw = 10
          , buildPoints = 6
          , specialProperties = []
          }
        , { name = "Flak Thrower"
          , range = Short
          , weaponClass = Light
          , weaponType = DirectFire
          , damage = Just ( 3, 4 )
          , powerDraw = 10
          , buildPoints = 5
          , specialProperties = [ Point 8 ]
          }
        , { name = "Gyrolaser"
          , range = Short
          , weaponClass = Light
          , weaponType = DirectFire
          , damage = Just ( 1, 8 )
          , powerDraw = 10
          , buildPoints = 3
          , specialProperties = [ BroadArc ]
          }
        , { name = "Laser Net"
          , range = Short
          , weaponClass = Light
          , weaponType = DirectFire
          , damage = Just ( 2, 5 )
          , powerDraw = 10
          , buildPoints = 9
          , specialProperties = [ Point 10 ]
          }
        , { name = "Laser EMP Cannon"
          , range = Short
          , weaponClass = Light
          , weaponType = DirectFire
          , damage = Nothing
          , powerDraw = 10
          , buildPoints = 8
          , specialProperties = [ Emp ]
          }
        , { name = "Light Laser Cannon"
          , range = Short
          , weaponClass = Light
          , weaponType = DirectFire
          , damage = Just ( 2, 4 )
          , powerDraw = 5
          , buildPoints = 2
          , specialProperties = []
          }
        , { name = "Light Particle Beam"
          , range = Weapon.Medium
          , weaponClass = Light
          , weaponType = DirectFire
          , damage = Just ( 3, 6 )
          , powerDraw = 10
          , buildPoints = 10
          , specialProperties = []
          }
        , { name = "Light Plasma Cannon"
          , range = Short
          , weaponClass = Light
          , weaponType = DirectFire
          , damage = Just ( 2, 12 )
          , powerDraw = 10
          , buildPoints = 12
          , specialProperties = []
          }
        , { name = "Mining Laser"
          , range = Short
          , weaponClass = Light
          , weaponType = DirectFire
          , damage = Just ( 2, 6 )
          , powerDraw = 10
          , buildPoints = 5
          , specialProperties = [ Burrowing ]
          }

        -- Light Tracking
        , { name = "High Explosive Missile Launcher"
          , range = Long
          , weaponClass = Light
          , weaponType = Tracking 12
          , damage = Just ( 4, 8 )
          , powerDraw = 10
          , buildPoints = 4
          , specialProperties = [ LimitedFire 5 ]
          }
        , { name = "Light Plasma Torpedo Launcher"
          , range = Long
          , weaponClass = Light
          , weaponType = Tracking 14
          , damage = Just ( 3, 8 )
          , powerDraw = 5
          , buildPoints = 5
          , specialProperties = [ LimitedFire 5 ]
          }
        , { name = "Light Torpedo Launcher"
          , range = Long
          , weaponClass = Light
          , weaponType = Tracking 16
          , damage = Just ( 2, 8 )
          , powerDraw = 5
          , buildPoints = 4
          , specialProperties = []
          }
        , { name = "Micromissile Battery"
          , range = Long
          , weaponClass = Light
          , weaponType = Tracking 10
          , damage = Just ( 2, 6 )
          , powerDraw = 10
          , buildPoints = 3
          , specialProperties = [ Array, LimitedFire 5 ]
          }
        , { name = "Tactical Nuclear Missile Launcher"
          , range = Long
          , weaponClass = Light
          , weaponType = Tracking 10
          , damage = Just ( 5, 8 )
          , powerDraw = 10
          , buildPoints = 5
          , specialProperties = [ Irradiate Low, LimitedFire 5 ]
          }

        -- Heavy Direct
        , { name = "Graser"
          , range = Short
          , weaponClass = Heavy
          , weaponType = DirectFire
          , damage = Just ( 7, 10 )
          , powerDraw = 40
          , buildPoints = 35
          , specialProperties = [ Irradiate MediumIrradiation ]
          }
        , { name = "Gravity Gun"
          , range = Weapon.Medium
          , weaponClass = Heavy
          , weaponType = DirectFire
          , damage = Just ( 6, 6 )
          , powerDraw = 40
          , buildPoints = 30
          , specialProperties = [ TractorBeam ]
          }
        , { name = "Heavy EMP Cannon"
          , range = Weapon.Medium
          , weaponClass = Heavy
          , weaponType = DirectFire
          , damage = Nothing
          , powerDraw = 30
          , buildPoints = 24
          , specialProperties = [ Emp ]
          }
        , { name = "Heavy Laser Array"
          , range = Short
          , weaponClass = Heavy
          , weaponType = DirectFire
          , damage = Just ( 6, 4 )
          , powerDraw = 15
          , buildPoints = 10
          , specialProperties = [ Array ]
          }
        , { name = "Heavy Laser Cannon"
          , range = Weapon.Medium
          , weaponClass = Heavy
          , weaponType = DirectFire
          , damage = Just ( 4, 8 )
          , powerDraw = 10
          , buildPoints = 8
          , specialProperties = []
          }
        , { name = "Heavy Laser Net"
          , range = Short
          , weaponClass = Heavy
          , weaponType = DirectFire
          , damage = Just ( 5, 6 )
          , powerDraw = 15
          , buildPoints = 12
          , specialProperties = [ Point 12 ]
          }
        , { name = "Maser"
          , range = Long
          , weaponClass = Heavy
          , weaponType = DirectFire
          , damage = Just ( 6, 10 )
          , powerDraw = 35
          , buildPoints = 22
          , specialProperties = []
          }
        , { name = "Particle Beam"
          , range = Long
          , weaponClass = Heavy
          , weaponType = DirectFire
          , damage = Just ( 8, 6 )
          , powerDraw = 25
          , buildPoints = 15
          , specialProperties = []
          }
        , { name = "Persistent Particle Beam"
          , range = Long
          , weaponClass = Heavy
          , weaponType = DirectFire
          , damage = Just ( 10, 6 )
          , powerDraw = 40
          , buildPoints = 25
          , specialProperties = []
          }
        , { name = "Plasma Cannon"
          , range = Weapon.Medium
          , weaponClass = Heavy
          , weaponType = DirectFire
          , damage = Just ( 5, 12 )
          , powerDraw = 30
          , buildPoints = 20
          , specialProperties = []
          }
        , { name = "Railgun"
          , range = Long
          , weaponClass = Heavy
          , weaponType = DirectFire
          , damage = Just ( 8, 4 )
          , powerDraw = 20
          , buildPoints = 15
          , specialProperties = []
          }
        , { name = "Twin Laser"
          , range = Long
          , weaponClass = Heavy
          , weaponType = DirectFire
          , damage = Just ( 5, 8 )
          , powerDraw = 15
          , buildPoints = 12
          , specialProperties = []
          }
        , { name = "X-Laser Cannon"
          , range = Long
          , weaponClass = Heavy
          , weaponType = DirectFire
          , damage = Just ( 8, 6 )
          , powerDraw = 40
          , buildPoints = 35
          , specialProperties = [ Line ]
          }

        -- Heavy Tracking
        , { name = "Heavy Antimatter Missile Launcher"
          , range = Long
          , weaponClass = Heavy
          , weaponType = Tracking 8
          , damage = Just ( 10, 10 )
          , powerDraw = 15
          , buildPoints = 12
          , specialProperties = [ LimitedFire 5 ]
          }
        , { name = "Heavy Nuclear Missile Launcher"
          , range = Long
          , weaponClass = Heavy
          , weaponType = Tracking 10
          , damage = Just ( 10, 8 )
          , powerDraw = 15
          , buildPoints = 12
          , specialProperties = [ Irradiate MediumIrradiation, LimitedFire 5 ]
          }
        , { name = "Heavy Plasma Torpedo Launcher"
          , range = Long
          , weaponClass = Heavy
          , weaponType = Tracking 12
          , damage = Just ( 5, 10 )
          , powerDraw = 10
          , buildPoints = 10
          , specialProperties = [ LimitedFire 5 ]
          }
        , { name = "Heavy Torpedo Launcher"
          , range = Long
          , weaponClass = Heavy
          , weaponType = Tracking 14
          , damage = Just ( 5, 8 )
          , powerDraw = 10
          , buildPoints = 8
          , specialProperties = [ LimitedFire 5 ]
          }

        --TODO: Capital Weapons
        ]



--shields


shields : KS.KeyedSet String Shields
shields =
    KS.fromList
        .name
        [ { name = "Basic Shields 10"
          , shieldPoints = 10
          , regenPerMinute = 1
          , powerDraw = 5
          , buildPoints = 2
          }
        , { name = "Basic Shields 20"
          , shieldPoints = 20
          , regenPerMinute = 1
          , powerDraw = 10
          , buildPoints = 3
          }
        , { name = "Basic Shields 30"
          , shieldPoints = 30
          , regenPerMinute = 1
          , powerDraw = 15
          , buildPoints = 4
          }
        , { name = "Basic Shields 40"
          , shieldPoints = 40
          , regenPerMinute = 1
          , powerDraw = 15
          , buildPoints = 5
          }
        , { name = "Light Shields 50"
          , shieldPoints = 50
          , regenPerMinute = 2
          , powerDraw = 20
          , buildPoints = 6
          }
        , { name = "Light Shields 60"
          , shieldPoints = 60
          , regenPerMinute = 2
          , powerDraw = 20
          , buildPoints = 8
          }
        , { name = "Light Shields 70"
          , shieldPoints = 70
          , regenPerMinute = 2
          , powerDraw = 25
          , buildPoints = 10
          }
        , { name = "Light Shields 80"
          , shieldPoints = 80
          , regenPerMinute = 2
          , powerDraw = 30
          , buildPoints = 12
          }
        , { name = "Medium Shields 90"
          , shieldPoints = 90
          , regenPerMinute = 4
          , powerDraw = 30
          , buildPoints = 13
          }
        , { name = "Medium Shields 100"
          , shieldPoints = 100
          , regenPerMinute = 4
          , powerDraw = 30
          , buildPoints = 15
          }
        , { name = "Medium Shields 120"
          , shieldPoints = 120
          , regenPerMinute = 4
          , powerDraw = 35
          , buildPoints = 17
          }
        , { name = "Medium Shields 140"
          , shieldPoints = 140
          , regenPerMinute = 8
          , powerDraw = 40
          , buildPoints = 18
          }
        , { name = "Medium Shields 160"
          , shieldPoints = 160
          , regenPerMinute = 8
          , powerDraw = 45
          , buildPoints = 20
          }
        , { name = "Medium Shields 200"
          , shieldPoints = 200
          , regenPerMinute = 8
          , powerDraw = 50
          , buildPoints = 22
          }
        , { name = "Heavy Shields 240"
          , shieldPoints = 240
          , regenPerMinute = 16
          , powerDraw = 55
          , buildPoints = 23
          }
        , { name = "Heavy Shields 280"
          , shieldPoints = 280
          , regenPerMinute = 16
          , powerDraw = 60
          , buildPoints = 25
          }
        , { name = "Heavy Shields 320"
          , shieldPoints = 320
          , regenPerMinute = 16
          , powerDraw = 70
          , buildPoints = 27
          }
        , { name = "Heavy Shields 360"
          , shieldPoints = 360
          , regenPerMinute = 32
          , powerDraw = 80
          , buildPoints = 28
          }
        , { name = "Heavy Shields 420"
          , shieldPoints = 420
          , regenPerMinute = 32
          , powerDraw = 90
          , buildPoints = 30
          }
        , { name = "Heavy Shields 480"
          , shieldPoints = 480
          , regenPerMinute = 32
          , powerDraw = 110
          , buildPoints = 32
          }
        , { name = "Superior Shields 540"
          , shieldPoints = 540
          , regenPerMinute = 64
          , powerDraw = 130
          , buildPoints = 35
          }
        , { name = "Superior Shields 600"
          , shieldPoints = 600
          , regenPerMinute = 64
          , powerDraw = 160
          , buildPoints = 40
          }
        ]


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
