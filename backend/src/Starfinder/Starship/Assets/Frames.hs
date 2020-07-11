{-# LANGUAGE OverloadedStrings #-}
module Starfinder.Starship.Assets.Frames where

import qualified Data.KeyedSet as KS
import Data.Text (Text)
import Starfinder.Starship.Arc (Arc(..))
import Starfinder.Starship.Frame (Frame(..), Maneuverability(..))
import Starfinder.Starship.Size (Size(..))
import Starfinder.Starship.Weapon
       (ArcMountClass(..), Class(..), TurretMountClass(..))

frames :: KS.KeyedSet Text Frame
frames =
    KS.fromList
        name
        [ Frame
          { name = "Medium Transport"
          , size = Medium
          , maneuverability = Average
          , baseHitPoints = 70
          , hitPointsIncrement = 15
          , damageThreshold = 0
          , arcMounts =
                Arc
                { forward = [ArcMountClass Heavy, ArcMountClass Light]
                , aft = [ArcMountClass Light]
                , portSide = []
                , starboard = []
                }
          , turretMounts = [TurretMountClass Light, TurretMountClass Light]
          , maxExpansionBays = 5
          , minimumCrew = 1
          , maximumCrew = 6
          , listedBuildPoints = 15
          }
        , Frame
          { name = "Medium Explorer"
          , size = Medium
          , maneuverability = Good
          , baseHitPoints = 55
          , hitPointsIncrement = 10
          , damageThreshold = 0
          , arcMounts =
                Arc
                { forward = [ArcMountClass Light]
                , aft = []
                , portSide = [ArcMountClass Light]
                , starboard = [ArcMountClass Light]
                }
          , turretMounts = [TurretMountClass Light]
          , maxExpansionBays = 4
          , minimumCrew = 1
          , maximumCrew = 6
          , listedBuildPoints = 12
          }
        ]
