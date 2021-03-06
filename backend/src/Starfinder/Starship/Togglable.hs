{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Starfinder.Starship.Togglable (Togglable(..), extract) where

import Data.Hashable (Hashable)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Starfinder.Starship.DrawsPower (DrawsPower(..))
import Starfinder.Starship.CostsBuildPoints (CostsBuildPoints(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))

data Togglable a = Togglable
  { isOn :: Bool
  , toggled :: a
  }
  deriving (Functor, Generic, Show, Eq)

instance Hashable a => Hashable (Togglable a)

instance Arbitrary a => Arbitrary (Togglable a) where
  arbitrary = Togglable <$> arbitrary <*> arbitrary

instance FromJSON a => FromJSON (Togglable a)
instance ToJSON a => ToJSON (Togglable a)

instance Foldable Togglable where
  foldMap f Togglable { toggled } =
    f toggled

instance Traversable Togglable where
  traverse f Togglable { toggled, isOn } =
    Togglable isOn <$> f toggled

extract :: Togglable a -> a
extract = toggled


instance DrawsPower a => DrawsPower (Togglable a) where
  getPowerDraw Togglable { isOn, toggled } =
    if isOn then
      getPowerDraw toggled
    else
      0

instance CostsBuildPoints a => CostsBuildPoints (Togglable a) where
  getBuildPoints Togglable { toggled } = getBuildPoints toggled
