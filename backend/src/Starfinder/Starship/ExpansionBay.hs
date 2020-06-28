{-# LANGUAGE DeriveGeneric #-}
module Starfinder.Starship.ExpansionBay (ExpansionBay(..), getExpansionBaysUsed, getSizeConstraints, isValidSize) where

import Starfinder.Starship.Size (Size(..))
import Starfinder.Starship.DrawsPower (DrawsPower(..))
import Starfinder.Starship.CostsBuildPoints (CostsBuildPoints(..))
import Data.Set (Set, member, fromList)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements, oneof)


data ExpansionBay
    = ArcaneLaboratory
    | CargoHold
    | EscapePods
      -- TODO: Guest quarters have the same possible designations
      -- as crew quarters: Common/Good/Luxurious
      -- This is useful if we want to determine the maximum number
      -- of guests that can be transported.
    | GuestQuarters
    | HangarBay
    | LifeBoats
    | MedicalBay
    | PassengerSeating
    | PowerCoreHousing
    | RecreationSuiteGym
    | RecreationSuiteTrivedDen
    | RecreationSuiteHac
      -- TODO: there are two different types of science lab
    | ScienceLab
    | SealedEnvironmentChamber
    | ShuttleBay
    | SmugglerCompartment Int
    | SynthesisBay
    | TechWorkshop
    deriving (Show, Eq, Ord, Generic, Read)

instance Arbitrary ExpansionBay where
  arbitrary = oneof [SmugglerCompartment <$> arbitrary, elements
    [ ArcaneLaboratory
    , CargoHold
    , EscapePods
    , GuestQuarters
    , HangarBay
    , LifeBoats
    , MedicalBay
    , PassengerSeating
    , PowerCoreHousing
    , RecreationSuiteGym
    , RecreationSuiteTrivedDen
    , RecreationSuiteHac
    , ScienceLab
    , SealedEnvironmentChamber
    , ShuttleBay
    , SynthesisBay
    , TechWorkshop
    ]]


instance FromJSON ExpansionBay
instance ToJSON ExpansionBay

data ExpansionBayCost = ExpansionBayCost
    { powerDraw :: Int
    , buildPoints :: Int
    , expansionBaysUsed :: Int
    , sizeConstraints :: Maybe (Set Size)
    }


getExpansionBayCost :: ExpansionBay -> ExpansionBayCost
getExpansionBayCost expansionBay =
    case expansionBay of
        ArcaneLaboratory ->
            ExpansionBayCost 1 1 1 Nothing

        CargoHold ->
            ExpansionBayCost 0 0 1 Nothing

        EscapePods ->
            ExpansionBayCost 2 1 1 Nothing

        GuestQuarters ->
            ExpansionBayCost 1 1 1 Nothing

        HangarBay ->
            ExpansionBayCost 30 10 4 (Just $ fromList [ Gargantuan, Colossal ])

        LifeBoats ->
            ExpansionBayCost 5 3 1 Nothing

        MedicalBay ->
            ExpansionBayCost 4 8 1 Nothing

        PassengerSeating ->
            ExpansionBayCost 0 0 1 Nothing

        PowerCoreHousing ->
            ExpansionBayCost 0 10 1 (Just $ fromList [ Medium, Large, Huge, Gargantuan, Colossal ])

        RecreationSuiteGym ->
            ExpansionBayCost 0 1 1 Nothing

        RecreationSuiteTrivedDen ->
            ExpansionBayCost 1 1 1 Nothing

        RecreationSuiteHac ->
            ExpansionBayCost 3 1 1 Nothing

        ScienceLab ->
            ExpansionBayCost 2 1 1 Nothing

        SealedEnvironmentChamber ->
            ExpansionBayCost 2 1 1 Nothing

        ShuttleBay ->
            ExpansionBayCost 10 4 2 (Just $ fromList [ Huge, Gargantuan, Colossal ])

        SmugglerCompartment dc ->
            ExpansionBayCost ((dc - 1) `div` 5 + 1) ((dc - 1) `div` 5 - 1) 1 Nothing

        SynthesisBay ->
            ExpansionBayCost 2 1 1 Nothing

        TechWorkshop ->
            ExpansionBayCost 3 1 1 Nothing

instance DrawsPower ExpansionBay where
    getPowerDraw = powerDraw . getExpansionBayCost


instance CostsBuildPoints ExpansionBay where
    getBuildPoints = buildPoints . getExpansionBayCost

getExpansionBaysUsed :: ExpansionBay -> Int
getExpansionBaysUsed =
    expansionBaysUsed . getExpansionBayCost


--Type class?  Is Size Constrained?
getSizeConstraints :: ExpansionBay -> Maybe (Set Size)
getSizeConstraints =
   sizeConstraints . getExpansionBayCost


isValidSize :: Size -> ExpansionBay -> Bool
isValidSize size bay =
    fromMaybe True
      $ fmap (member size)
      $ getSizeConstraints bay
