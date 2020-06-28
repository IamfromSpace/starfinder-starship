{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Control.Lens (Lens', _1, _2, set, view)
import Control.Lens.At (at)
import Control.Monad ((>=>))
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap, fromList, lookup)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.Scientific (FPFormat(..), Scientific, formatScientific)
import Data.Set (Set, member)
import Data.Text (Text, pack, unpack)
import Data.Text.Arbitrary
import Network.AWS.DynamoDB.Types
       (AttributeValue, attributeValue, avBOOL, avL, avM, avN, avNULL,
        avS, avSS)
import Prelude hiding (lookup)
import Starfinder.Starship.Arc (Arc(..))
import Starfinder.Starship.Build
       (Armor(..), Build(..), CrewQuarters, DefensiveCountermeasures(..),
        DriftEngine, PowerCoreUnits(..), Sensor(..), Thrusters(..))
import Starfinder.Starship.Computer (Computer(..))
import Starfinder.Starship.CostsBuildPoints (CostsBuildPoints(..))
import Starfinder.Starship.DefenseLevel (DefenseLevel)
import Starfinder.Starship.ExpansionBay (ExpansionBay)
import Starfinder.Starship.Togglable (Togglable(..))
import Starfinder.Starship.Weapon (Range)
import Text.Read (readMaybe)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

data ETagged a =
    ETagged Text
            a deriving (Show, Eq)

instance Arbitrary a => Arbitrary (ETagged a) where
  arbitrary = ETagged <$> arbitrary <*> arbitrary

data OwnedBy a =
    OwnedBy Text
            a deriving (Show, Eq)

instance Arbitrary a => Arbitrary (OwnedBy a) where
  arbitrary = OwnedBy <$> arbitrary <*> arbitrary

class ToDynamoDbAttrValue a where
    toAttrValue :: a -> AttributeValue

instance ToDynamoDbAttrValue AttributeValue where
    toAttrValue x = x

instance ToDynamoDbAttrValue Text where
    toAttrValue s = set avS (Just s) attributeValue

instance ToDynamoDbAttrValue Scientific where
    toAttrValue n = set avN (Just $ pack $ show n) attributeValue

instance ToDynamoDbAttrValue Int where
    toAttrValue n = set avN (Just $ pack $ show n) attributeValue

instance ToDynamoDbAttrValue a => ToDynamoDbAttrValue (HashMap Text a) where
    toAttrValue o = set avM (fmap toAttrValue o) attributeValue

instance ToDynamoDbAttrValue a => ToDynamoDbAttrValue (Maybe a) where
    toAttrValue x =
        fromMaybe (set avNULL (Just True) attributeValue) $ fmap toAttrValue x

instance ToDynamoDbAttrValue a => ToDynamoDbAttrValue [a] where
    toAttrValue xs = set avL (fmap toAttrValue xs) attributeValue

instance ToDynamoDbAttrValue Bool where
    toAttrValue b = set avBOOL (Just b) attributeValue

instance ToDynamoDbAttrValue a => ToDynamoDbAttrValue (Togglable a) where
    toAttrValue Togglable {toggled, isOn} =
        toAttrValue $
        fromList
            [("isOn" :: Text, toAttrValue isOn), ("togglable", toAttrValue toggled)]

instance ToDynamoDbAttrValue PowerCoreUnits where
    toAttrValue = toAttrValue . getUnits

instance ToDynamoDbAttrValue Thrusters where
    toAttrValue = toAttrValue . getSpeed

instance ToDynamoDbAttrValue Armor where
    toAttrValue = toAttrValue . pack . show . getDefenseLevelA

instance ToDynamoDbAttrValue Computer where
    toAttrValue Computer {nodes, bonus} =
        toAttrValue $
        fromList
            [ ("nodes" :: Text, nodes)
            , ("bonus", bonus)
            ]

instance ToDynamoDbAttrValue CrewQuarters where
    toAttrValue = toAttrValue . pack . show

instance ToDynamoDbAttrValue DefensiveCountermeasures where
    toAttrValue = toAttrValue . pack . show . getDefenseLevel

instance ToDynamoDbAttrValue DriftEngine where
    toAttrValue = toAttrValue . pack . show

instance ToDynamoDbAttrValue ExpansionBay where
    toAttrValue = toAttrValue . pack . show

instance ToDynamoDbAttrValue Range where
    toAttrValue = toAttrValue . pack . show

instance ToDynamoDbAttrValue Sensor where
    toAttrValue Sensor {range, bonus} =
        toAttrValue $
        fromList
            [ ("range" :: Text, toAttrValue range)
            , ("bonus", toAttrValue bonus)
            ]

instance ToDynamoDbAttrValue a => ToDynamoDbAttrValue (Arc a) where
    toAttrValue Arc {forward, aft, portSide, starboard} =
        toAttrValue $
        fromList
            [ ("forward" :: Text, forward)
            , ("aft", aft)
            , ("portSide", portSide)
            , ("starboard", starboard)
            ]

-- TODO: Improved indexing strategy so composition is always preserved, and then
-- indexes are always lenses (getters) inside and can be fully typed and
-- separated.
instance ToDynamoDbAttrValue (ETagged (OwnedBy (Build Text Text Text))) where
    toAttrValue (ETagged eTag (OwnedBy userId build@Build { frame
                                            , powerCoreUnits
                                            , thrusters
                                            , armor
                                            , computer
                                            , crewQuarters
                                            , defensiveCountermeasures
                                            , driftEngine
                                            , name
                                            , expansionBays
                                            , sensors
                                            , arcWeapons
                                            , turretWeapons
                                            , shields
                                            })) =
        toAttrValue $
        fromList
            [ ("HASH1" :: Text, toAttrValue userId)
            , ("RANGE1.1", toAttrValue name)
          --, ("RANGE1.2", Number $ getBuildPoints build) (sorting by build points would be great!  But there is no insance for Build Text Text Text...)
            , ("eTag", toAttrValue eTag)
            , ("name", toAttrValue name)
            , ("frame", toAttrValue frame)
            , ("powerCoreUnits", toAttrValue powerCoreUnits)
            , ("thrusters", toAttrValue thrusters)
            , ("armor", toAttrValue armor)
            , ("computer", toAttrValue computer)
            , ("crewQuarters", toAttrValue crewQuarters)
            , ("defensiveCountermeasures", toAttrValue defensiveCountermeasures)
            , ("driftEngine", toAttrValue driftEngine)
            , ("expansionBays", toAttrValue expansionBays)
            , ("sensor", toAttrValue sensors)
            , ("arcWeapons", toAttrValue arcWeapons)
            , ("turretWeapons", toAttrValue turretWeapons)
            , ("shields", toAttrValue shields)
            ]

ownedReferencedBuildToItem ::
       ETagged (OwnedBy (Build Text Text Text)) -> HashMap Text AttributeValue
ownedReferencedBuildToItem
  -- Since we known that this type is always a HashMap, we're good,
  -- but this does not work across _all_ AttributeValues, because if
  -- it's a String/Number/etc, this will just return an empty HashMap
 = view avM . toAttrValue

-- TODO: A Maybe makes this almost impossible to debug in the face of a complex
-- data type.  Should be at least an Either String, if not something better.
class FromDynamoDbAttrValue a where
    fromAttrValue :: AttributeValue -> Maybe a

instance FromDynamoDbAttrValue Text where
    fromAttrValue = view avS

instance FromDynamoDbAttrValue a => FromDynamoDbAttrValue [a] where
    fromAttrValue = traverse fromAttrValue . view avL

instance FromDynamoDbAttrValue a => FromDynamoDbAttrValue (Togglable a) where
    fromAttrValue =
        (\hashMap ->
             Togglable <$> (view avBOOL =<< lookup "isOn" hashMap) <*>
             (fromAttrValue =<< lookup "togglable" hashMap)) .
        view avM

instance FromDynamoDbAttrValue a => FromDynamoDbAttrValue (Arc a) where
    fromAttrValue =
        (\hashMap ->
             Arc <$> (fromAttrValue =<< lookup "forward" hashMap) <*>
             (fromAttrValue =<< lookup "aft" hashMap) <*>
             (fromAttrValue =<< lookup "portSide" hashMap) <*>
             (fromAttrValue =<< lookup "starboard" hashMap)) .
        view avM

maybeHead :: [a] -> Maybe a
maybeHead (a:_) = Just a
maybeHead _ = Nothing

instance FromDynamoDbAttrValue Int where
    fromAttrValue = view avN >=> (readMaybe . unpack)

instance FromDynamoDbAttrValue PowerCoreUnits where
    fromAttrValue = (fmap PowerCoreUnits) . fromAttrValue

instance FromDynamoDbAttrValue Thrusters where
    fromAttrValue = (fmap Thrusters) . fromAttrValue

instance FromDynamoDbAttrValue DefenseLevel where
    fromAttrValue = view avS >=> (readMaybe . unpack)

instance FromDynamoDbAttrValue Armor where
    fromAttrValue = (fmap Armor) . fromAttrValue

instance FromDynamoDbAttrValue Computer where
    fromAttrValue =
        (\hashMap ->
             Computer <$> (fromAttrValue =<< lookup "bonus" hashMap) <*>
             (fromAttrValue =<< lookup "nodes" hashMap)) .
        view avM

instance FromDynamoDbAttrValue CrewQuarters where
    fromAttrValue = view avS >=> (readMaybe . unpack)

instance FromDynamoDbAttrValue DefensiveCountermeasures where
    fromAttrValue = (fmap DefensiveCountermeasures) . fromAttrValue

instance FromDynamoDbAttrValue DriftEngine where
    fromAttrValue = view avS >=> (readMaybe . unpack)

instance FromDynamoDbAttrValue ExpansionBay where
    fromAttrValue = view avS >=> (readMaybe . unpack)

instance FromDynamoDbAttrValue Range where
    fromAttrValue = view avS >=> (readMaybe . unpack)

instance FromDynamoDbAttrValue Sensor where
    fromAttrValue =
        (\hashMap ->
             Sensor <$> (fromAttrValue =<< lookup "range" hashMap) <*>
             (fromAttrValue =<< lookup "bonus" hashMap)) .
        view avM

instance FromDynamoDbAttrValue (ETagged (OwnedBy (Build Text Text Text))) where
    fromAttrValue =
        (\hashMap ->
             ETagged <$> (fromAttrValue =<< lookup "eTag" hashMap) <*>
              (OwnedBy <$> (fromAttrValue =<< lookup "HASH1" hashMap) <*>
              (Build <$> (fromAttrValue =<< lookup "name" hashMap) <*>
               (fromAttrValue =<< lookup "frame" hashMap) <*>
               (fromAttrValue =<< lookup "powerCoreUnits" hashMap) <*>
               (fromAttrValue =<< lookup "thrusters" hashMap) <*>
               (fromAttrValue <$> lookup "armor" hashMap) <*>
               (fromAttrValue =<< lookup "computer" hashMap) <*>
               (fromAttrValue =<< lookup "crewQuarters" hashMap) <*>
               (fromAttrValue <$> lookup "defensiveCountermeasures" hashMap) <*>
               (fromAttrValue <$> lookup "driftEngine" hashMap) <*>
               (fromAttrValue =<< lookup "expansionBays" hashMap) <*>
               (fromAttrValue =<< lookup "sensor" hashMap) <*>
               (fromAttrValue =<< lookup "arcWeapons" hashMap) <*>
               (fromAttrValue =<< lookup "turretWeapons" hashMap) <*>
               (fromAttrValue =<< lookup "shields" hashMap)))) .
        view avM
