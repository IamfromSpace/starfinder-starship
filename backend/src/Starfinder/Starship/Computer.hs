{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Starfinder.Starship.Computer (Computer(..), getBuildPoints, getPowerDraw) where

import Starfinder.Starship.DrawsPower (DrawsPower(..))
import Starfinder.Starship.CostsBuildPoints (CostsBuildPoints(..))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)


data Computer = Computer
    { bonus :: Int
    , nodes :: Int
    } deriving (Generic)

instance FromJSON Computer
instance ToJSON Computer

instance DrawsPower Computer where
    getPowerDraw Computer { bonus, nodes } =
        if bonus > 0 && nodes > 0 then
            bonus * 5 + 5

        else
            0

instance CostsBuildPoints Computer where
    getBuildPoints Computer { bonus, nodes } =
        bonus * bonus * nodes
