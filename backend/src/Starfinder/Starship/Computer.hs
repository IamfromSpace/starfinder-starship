{-# LANGUAGE NamedFieldPuns #-}
module Starfinder.Starship.Computer (Computer, getBuildPoints, getPowerDraw) where

import Starfinder.Starship.DrawsPower (DrawsPower(..))
import Starfinder.Starship.CostsBuildPoints (CostsBuildPoints(..))


data Computer = Computer
    { bonus :: Int
    , nodes :: Int
    }

instance DrawsPower Computer where
    getPowerDraw Computer { bonus, nodes } =
        if bonus > 0 && nodes > 0 then
            bonus * 5 + 5

        else
            0

instance CostsBuildPoints Computer where
    getBuildPoints Computer { bonus, nodes } =
        bonus * bonus * nodes
