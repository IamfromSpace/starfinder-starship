{-# LANGUAGE OverloadedStrings #-}

module Starfinder.Starship.Assets.Weapons where

import qualified Data.KeyedSet as KS
import Data.Text (Text)
import Starfinder.Starship.Size (Size(..))
import Starfinder.Starship.Weapon
       (Weapon(..), Type(..), Range(..), ArcMountClass(..), Class(..), TurretMountClass(..))

weapons :: KS.KeyedSet Text Weapon
weapons =
    KS.fromList
        name
        [ Weapon
          { name = "Coilgun"
          , range = Long
          , weaponClass = Light
          , weaponType = DirectFire True
          , damage = Just (4, 4)
          , powerDraw = 10
          , buildPoints = 6
          , specialProperties = mempty
          }
        ]
