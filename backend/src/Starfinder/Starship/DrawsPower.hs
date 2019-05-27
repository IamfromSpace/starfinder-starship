module Starfinder.Starship.DrawsPower (DrawsPower(..)) where

class DrawsPower a where
  getPowerDraw :: a -> Int


instance DrawsPower a => DrawsPower (Maybe a) where
    getPowerDraw (Just a) = getPowerDraw a
    getPowerDraw Nothing = 0


instance DrawsPower a => DrawsPower [a] where
    getPowerDraw = sum . fmap getPowerDraw
