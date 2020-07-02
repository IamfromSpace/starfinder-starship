{-# LANGUAGE OverloadedStrings #-}

module Starfinder.Starship.Assets.Weapons where

import qualified Data.KeyedSet as KS
import Data.Text (Text)
import Starfinder.Starship.Size (Size(..))
import qualified Starfinder.Starship.ReferencedWeapon as RW
import Starfinder.Starship.Weapon
       (Weapon(..), Type(..), Range(..), ArcMountClass(..), Class(..), TurretMountClass(..))

dereferenceWeapon :: RW.ReferencedWeapon -> Maybe (Weapon Bool)
dereferenceWeapon rw = do
  weapon <- KS.lookup (RW.name rw) weapons
  case weaponType weapon of
    DirectFire _ -> Just $ fmap (const (RW.isLinked rw)) weapon
    Tracking _ -> Nothing

weapons :: KS.KeyedSet Text (Weapon ())
weapons =
    KS.fromList
        name
        [ Weapon
          { name = "Coilgun"
          , range = Long
          , weaponClass = Light
          , weaponType = DirectFire ()
          , damage = Just (4, 4)
          , powerDraw = 10
          , buildPoints = 6
          , specialProperties = mempty
          }
        ]
