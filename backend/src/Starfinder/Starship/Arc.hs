{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Starfinder.Starship.Arc (AnArc(..), Arc(..), foldWithAnArc, getArc, getDegrees, mapWithAnArc, setArc, updateArc) where

import Starfinder.Starship.DrawsPower (DrawsPower(..))
import Starfinder.Starship.CostsBuildPoints (CostsBuildPoints(..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (toList)
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

data Arc a = Arc
    { forward :: a
    , aft :: a
    , portSide :: a
    , starboard :: a
    } deriving (Generic, Show, Eq)

instance Arbitrary a => Arbitrary (Arc a) where
  arbitrary = Arc <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance FromJSON a => FromJSON (Arc a)
instance ToJSON a => ToJSON (Arc a)

instance Functor Arc where
  fmap f Arc { forward, aft, portSide, starboard } =
    Arc (f forward) (f aft) (f portSide) (f starboard)

instance Applicative Arc where
  pure x = Arc x x x x
  Arc { forward = forwardF, aft = aftF, portSide = portSideF, starboard = starboardF } <*> Arc { forward, aft, portSide, starboard } =
    Arc (forwardF forward) (aftF aft) (portSideF portSide) (starboardF starboard)

instance Foldable Arc where
  foldMap f Arc { forward, aft, portSide, starboard } =
    f forward <> f aft <> f portSide <> f starboard

mapWithAnArc :: (AnArc -> a -> b) -> Arc a -> Arc b
mapWithAnArc fn Arc { forward, aft, portSide, starboard } =
  Arc
    (fn Forward forward)
    (fn Aft aft)
    (fn Port portSide)
    (fn Starboard starboard)

foldWithAnArc :: (AnArc -> a -> b -> b) -> b -> Arc a -> b
foldWithAnArc fn init Arc { forward, aft, portSide, starboard } =
    fn Starboard starboard
        $ fn Port portSide
        $ fn Aft aft
        $ fn Forward forward init

data AnArc
    = Forward
    | Aft
    | Port
    | Starboard

getDegrees :: AnArc -> Float
getDegrees arc =
    case arc of
        Forward ->
            0

        Aft ->
            180

        Port ->
            270

        Starboard ->
            90


updateArc :: (a -> a) -> AnArc -> Arc a -> Arc a
updateArc fn arc a@Arc { forward, aft, portSide, starboard } =
    case arc of
        Forward ->
            a { forward = fn forward }

        Aft ->
            a { aft = fn aft }

        Port ->
            a { portSide = fn portSide }

        Starboard ->
            a { starboard = fn starboard }


getArc :: AnArc -> Arc a -> a
getArc arc Arc { forward, aft, portSide, starboard } =
    case arc of
        Forward ->
            forward

        Aft ->
            aft

        Port ->
            portSide

        Starboard ->
            starboard


setArc :: (a -> a) -> AnArc -> Arc a -> Arc a
setArc = updateArc

instance DrawsPower a => DrawsPower (Arc a) where
    getPowerDraw = getPowerDraw . toList


instance CostsBuildPoints a => CostsBuildPoints (Arc a) where
    getBuildPoints = getBuildPoints . toList
