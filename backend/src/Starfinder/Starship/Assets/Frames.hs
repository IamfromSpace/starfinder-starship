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
          { name = "Tiny Fighter"
          , size = Tiny
          , maneuverability = Good
          , baseHitPoints = 35
          , hitPointsIncrement = 5
          , damageThreshold = 0
          , arcMounts =
                Arc
                { forward = [ArcMountClass Light]
                , aft = []
                , portSide = []
                , starboard = []
                }
          , turretMounts = []
          , maxExpansionBays = 3
          , minimumCrew = 1
          , maximumCrew = 4
          , listedBuildPoints = 6
          }
        , Frame
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
        , Frame
          { name = "Large Destroyer"
          , size = Large
          , maneuverability = Average
          , baseHitPoints = 150
          , hitPointsIncrement = 20
          , damageThreshold = 0
          , arcMounts =
                Arc
                { forward = [ArcMountClass Heavy, ArcMountClass Heavy]
                , aft = [ArcMountClass Light]
                , portSide = [ArcMountClass Light]
                , starboard = [ArcMountClass Light]
                }
          , turretMounts = [TurretMountClass Light]
          , maxExpansionBays = 4
          , minimumCrew = 6
          , maximumCrew = 20
          , listedBuildPoints = 30
          }
        ]
