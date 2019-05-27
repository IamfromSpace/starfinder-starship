{-# LANGUAGE DeriveFunctor #-}
module Starfinder.Starship.Togglable (Togglable(..), extract) where

import Starfinder.Starship.DrawsPower (DrawsPower(..))
import Starfinder.Starship.CostsBuildPoints (CostsBuildPoints(..))

data Togglable a = Togglable Bool a
  deriving (Functor)


extract :: Togglable a -> a
extract (Togglable _ a) = a


instance DrawsPower a => DrawsPower (Togglable a) where
  getPowerDraw (Togglable isOn toggled) =
    if isOn then
      getPowerDraw toggled
    else
      0

instance CostsBuildPoints a => CostsBuildPoints (Togglable a) where
  getBuildPoints (Togglable _ toggled) = getBuildPoints toggled
