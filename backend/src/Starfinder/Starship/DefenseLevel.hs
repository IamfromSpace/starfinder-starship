{-# LANGUAGE DeriveGeneric #-}
module Starfinder.Starship.DefenseLevel (DefenseLevel(..)) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)


data DefenseLevel
    = Mk1
    | Mk2
    | Mk3
    | Mk4
    | Mk5
    | Mk6
    | Mk7
    | Mk8
    | Mk9
    | Mk10
    | Mk11
    | Mk12
    | Mk13
    | Mk14
    | Mk15
    deriving (Show, Generic)

instance FromJSON DefenseLevel
instance ToJSON DefenseLevel
