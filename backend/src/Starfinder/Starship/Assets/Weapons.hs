{-# LANGUAGE OverloadedStrings #-}

module Starfinder.Starship.Assets.Weapons where

import qualified Data.KeyedSet as KS
import Data.Set as S
import Data.Text (Text)
import qualified Starfinder.Starship.ReferencedWeapon as RW
import Starfinder.Starship.Weapon
       (Class(..), Irradiation(..), Range(..), Type(..), Weapon(..),
        WeaponProperty(..))

dereferenceWeapon :: RW.ReferencedWeapon -> Maybe (Weapon Bool)
dereferenceWeapon rw = do
    weapon <- KS.lookup (RW.name rw) weapons
    case (weaponType weapon, (RW.isLinked rw)) of
        (Tracking _, True) ->
          Nothing
        (_, isLinked) -> Just $ fmap (const isLinked) weapon

weapons :: KS.KeyedSet Text (Weapon ())
weapons =
    KS.fromList
        name
        -- Light Direct
        [ Weapon
          { name = "Chain Cannon"
          , range = Short
          , weaponClass = Light
          , weaponType = DirectFire ()
          , damage = Just (6, 4)
          , powerDraw = 15
          , buildPoints = 15
          , specialProperties = S.fromList [Ripper]
          }
        , Weapon
          { name = "Coilgun"
          , range = Long
          , weaponClass = Light
          , weaponType = DirectFire ()
          , damage = Just (4, 4)
          , powerDraw = 10
          , buildPoints = 6
          , specialProperties = mempty
          }
        , Weapon
          { name = "Flak Thrower"
          , range = Short
          , weaponClass = Light
          , weaponType = DirectFire ()
          , damage = Just (3, 4)
          , powerDraw = 10
          , buildPoints = 5
          , specialProperties = S.fromList [Point 8]
          }
        , Weapon
          { name = "Gyrolaser"
          , range = Short
          , weaponClass = Light
          , weaponType = DirectFire ()
          , damage = Just (1, 8)
          , powerDraw = 10
          , buildPoints = 3
          , specialProperties = S.fromList [BroadArc]
          }
        , Weapon
          { name = "Laser Net"
          , range = Short
          , weaponClass = Light
          , weaponType = DirectFire ()
          , damage = Just (2, 5)
          , powerDraw = 10
          , buildPoints = 9
          , specialProperties = S.fromList [Point 10]
          }
        , Weapon
          { name = "Laser EMP Cannon"
          , range = Short
          , weaponClass = Light
          , weaponType = DirectFire ()
          , damage = Nothing
          , powerDraw = 10
          , buildPoints = 8
          , specialProperties = S.fromList [Emp]
          }
        , Weapon
          { name = "Light Laser Cannon"
          , range = Short
          , weaponClass = Light
          , weaponType = DirectFire ()
          , damage = Just (2, 4)
          , powerDraw = 5
          , buildPoints = 2
          , specialProperties = S.fromList []
          }
        , Weapon
          { name = "Light Particle Beam"
          , range = Medium
          , weaponClass = Light
          , weaponType = DirectFire ()
          , damage = Just (3, 6)
          , powerDraw = 10
          , buildPoints = 10
          , specialProperties = S.fromList []
          }
        , Weapon
          { name = "Light Plasma Cannon"
          , range = Short
          , weaponClass = Light
          , weaponType = DirectFire ()
          , damage = Just (2, 12)
          , powerDraw = 10
          , buildPoints = 12
          , specialProperties = S.fromList []
          }
        , Weapon
          { name = "Mining Laser"
          , range = Short
          , weaponClass = Light
          , weaponType = DirectFire ()
          , damage = Just (2, 6)
          , powerDraw = 10
          , buildPoints = 5
          , specialProperties = S.fromList [Burrowing]
          }
          -- Light Tracking
        , Weapon
          { name = "High Explosive Missile Launcher"
          , range = Long
          , weaponClass = Light
          , weaponType = Tracking 12
          , damage = Just (4, 8)
          , powerDraw = 10
          , buildPoints = 4
          , specialProperties = S.fromList [LimitedFire 5]
          }
        , Weapon
          { name = "Light Plasma Torpedo Launcher"
          , range = Long
          , weaponClass = Light
          , weaponType = Tracking 14
          , damage = Just (3, 8)
          , powerDraw = 5
          , buildPoints = 5
          , specialProperties = S.fromList [LimitedFire 5]
          }
        , Weapon
          { name = "Light Torpedo Launcher"
          , range = Long
          , weaponClass = Light
          , weaponType = Tracking 16
          , damage = Just (2, 8)
          , powerDraw = 5
          , buildPoints = 4
          , specialProperties = S.fromList []
          }
        , Weapon
          { name = "Micromissile Battery"
          , range = Long
          , weaponClass = Light
          , weaponType = Tracking 10
          , damage = Just (2, 6)
          , powerDraw = 10
          , buildPoints = 3
          , specialProperties = S.fromList [Array, LimitedFire 5]
          }
        , Weapon
          { name = "Tactical Nuclear Missile Launcher"
          , range = Long
          , weaponClass = Light
          , weaponType = Tracking 10
          , damage = Just (5, 8)
          , powerDraw = 10
          , buildPoints = 5
          , specialProperties = S.fromList [Irradiate Low, LimitedFire 5]
          }
          -- Heavy Direct
        , Weapon
          { name = "Graser"
          , range = Short
          , weaponClass = Heavy
          , weaponType = DirectFire ()
          , damage = Just (7, 10)
          , powerDraw = 40
          , buildPoints = 35
          , specialProperties = S.fromList [Irradiate MediumIrradiation]
          }
        , Weapon
          { name = "Gravity Gun"
          , range = Medium
          , weaponClass = Heavy
          , weaponType = DirectFire ()
          , damage = Just (6, 6)
          , powerDraw = 40
          , buildPoints = 30
          , specialProperties = S.fromList [TractorBeam]
          }
        , Weapon
          { name = "Heavy EMP Cannon"
          , range = Medium
          , weaponClass = Heavy
          , weaponType = DirectFire ()
          , damage = Nothing
          , powerDraw = 30
          , buildPoints = 24
          , specialProperties = S.fromList [Emp]
          }
        , Weapon
          { name = "Heavy Laser Array"
          , range = Short
          , weaponClass = Heavy
          , weaponType = DirectFire ()
          , damage = Just (6, 4)
          , powerDraw = 15
          , buildPoints = 10
          , specialProperties = S.fromList [Array]
          }
        , Weapon
          { name = "Heavy Laser Cannon"
          , range = Medium
          , weaponClass = Heavy
          , weaponType = DirectFire ()
          , damage = Just (4, 8)
          , powerDraw = 10
          , buildPoints = 8
          , specialProperties = S.fromList []
          }
        , Weapon
          { name = "Heavy Laser Net"
          , range = Short
          , weaponClass = Heavy
          , weaponType = DirectFire ()
          , damage = Just (5, 6)
          , powerDraw = 15
          , buildPoints = 12
          , specialProperties = S.fromList [Point 12]
          }
        , Weapon
          { name = "Maser"
          , range = Long
          , weaponClass = Heavy
          , weaponType = DirectFire ()
          , damage = Just (6, 10)
          , powerDraw = 35
          , buildPoints = 22
          , specialProperties = S.fromList []
          }
        , Weapon
          { name = "Particle Beam"
          , range = Long
          , weaponClass = Heavy
          , weaponType = DirectFire ()
          , damage = Just (8, 6)
          , powerDraw = 25
          , buildPoints = 15
          , specialProperties = S.fromList []
          }
        , Weapon
          { name = "Persistent Particle Beam"
          , range = Long
          , weaponClass = Heavy
          , weaponType = DirectFire ()
          , damage = Just (10, 6)
          , powerDraw = 40
          , buildPoints = 25
          , specialProperties = S.fromList []
          }
        , Weapon
          { name = "Plasma Cannon"
          , range = Medium
          , weaponClass = Heavy
          , weaponType = DirectFire ()
          , damage = Just (5, 12)
          , powerDraw = 30
          , buildPoints = 20
          , specialProperties = S.fromList []
          }
        , Weapon
          { name = "Railgun"
          , range = Long
          , weaponClass = Heavy
          , weaponType = DirectFire ()
          , damage = Just (8, 4)
          , powerDraw = 20
          , buildPoints = 15
          , specialProperties = S.fromList []
          }
        , Weapon
          { name = "Twin Laser"
          , range = Long
          , weaponClass = Heavy
          , weaponType = DirectFire ()
          , damage = Just (5, 8)
          , powerDraw = 15
          , buildPoints = 12
          , specialProperties = S.fromList []
          }
        , Weapon
          { name = "X-Laser Cannon"
          , range = Long
          , weaponClass = Heavy
          , weaponType = DirectFire ()
          , damage = Just (8, 6)
          , powerDraw = 40
          , buildPoints = 35
          , specialProperties = S.fromList [Line]
          }
          -- Heavy Tracking
        , Weapon
          { name = "Heavy Antimatter Missile Launcher"
          , range = Long
          , weaponClass = Heavy
          , weaponType = Tracking 8
          , damage = Just (10, 10)
          , powerDraw = 15
          , buildPoints = 12
          , specialProperties = S.fromList [LimitedFire 5]
          }
        , Weapon
          { name = "Heavy Nuclear Missile Launcher"
          , range = Long
          , weaponClass = Heavy
          , weaponType = Tracking 10
          , damage = Just (10, 8)
          , powerDraw = 15
          , buildPoints = 12
          , specialProperties = S.fromList [Irradiate MediumIrradiation, LimitedFire 5]
          }
        , Weapon
          { name = "Heavy Plasma Torpedo Launcher"
          , range = Long
          , weaponClass = Heavy
          , weaponType = Tracking 12
          , damage = Just (5, 10)
          , powerDraw = 10
          , buildPoints = 10
          , specialProperties = S.fromList [LimitedFire 5]
          }
        , Weapon
          { name = "Heavy Torpedo Launcher"
          , range = Long
          , weaponClass = Heavy
          , weaponType = Tracking 14
          , damage = Just (5, 8)
          , powerDraw = 10
          , buildPoints = 8
          , specialProperties = S.fromList [LimitedFire 5]
          }
          --TODO: Capital Weapons
        ]
