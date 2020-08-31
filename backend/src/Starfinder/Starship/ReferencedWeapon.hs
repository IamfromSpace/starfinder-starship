{-# LANGUAGE DeriveGeneric #-}

module Starfinder.Starship.ReferencedWeapon
    ( ReferencedWeapon(..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text.Arbitrary ()
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

data ReferencedWeapon = ReferencedWeapon
    { isLinked :: Bool
    , name :: Text
    } deriving (Generic, Show, Read, Eq)

instance Hashable ReferencedWeapon

instance Arbitrary ReferencedWeapon where
    arbitrary = ReferencedWeapon <$> arbitrary <*> arbitrary

instance FromJSON ReferencedWeapon

instance ToJSON ReferencedWeapon
