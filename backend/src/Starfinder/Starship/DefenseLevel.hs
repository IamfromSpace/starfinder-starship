{-# LANGUAGE DeriveGeneric #-}

module Starfinder.Starship.DefenseLevel
    ( DefenseLevel(..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements)

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
    deriving (Show, Read, Generic, Eq)

instance Hashable DefenseLevel

instance Arbitrary DefenseLevel where
    arbitrary =
        elements
            [ Mk1
            , Mk2
            , Mk3
            , Mk4
            , Mk5
            , Mk6
            , Mk7
            , Mk8
            , Mk9
            , Mk10
            , Mk11
            , Mk12
            , Mk13
            , Mk14
            , Mk15
            ]

instance FromJSON DefenseLevel

instance ToJSON DefenseLevel
