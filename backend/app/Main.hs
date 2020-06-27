{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Lens (Lens', _1, _2, set, view)
import Control.Lens.At (at)
import Control.Monad ((>=>))
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap, fromList, lookup)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Scientific (FPFormat(..), Scientific, formatScientific)
import Data.Set (Set, member)
import Data.Text (Text, pack, unpack)
import Network.AWS.DynamoDB.Types
       (AttributeValue, attributeValue, avBOOL, avL, avM, avN, avNULL,
        avS, avSS)
import Prelude hiding (lookup)
import Starfinder.Starship.Arc (Arc(..))
import Starfinder.Starship.Build
       (Armor(..), Build(..), CrewQuarters, DefensiveCounterMeasures(..),
        DriftEngine, PowerCoreUnits(..), Sensor(..), Thrusters(..))
import Starfinder.Starship.Computer (Computer(..))
import Starfinder.Starship.CostsBuildPoints (CostsBuildPoints(..))
import Starfinder.Starship.DefenseLevel (DefenseLevel)
import Starfinder.Starship.ExpansionBay (ExpansionBay)
import Starfinder.Starship.Togglable (Togglable(..))
import Starfinder.Starship.Weapon (Range)
import Text.Read (readMaybe)

data DynamoValue
    = String Text
    | Number Scientific
    | Object (HashMap Text DynamoValue)
    | Null
    | StringSet (Set Text)
    | List [DynamoValue]
    | Boolean Bool

toAttrValue :: DynamoValue -> AttributeValue
toAttrValue x =
    case x of
        String s -> set avS (Just s) attributeValue
        Number n -> set avS (Just $ pack $ show n) attributeValue
        Object o -> set avM (fmap toAttrValue o) attributeValue
        Null -> set avNULL (Just True) attributeValue
        StringSet s -> set avSS (toList s) attributeValue
        List xs -> set avL (fmap toAttrValue xs) attributeValue
        Boolean b -> set avBOOL (Just b) attributeValue

data OwnedBy a =
    OwnedBy Text
            a

class ToDynamoDbValue a where
    toValue :: a -> DynamoValue

instance ToDynamoDbValue DynamoValue where
    toValue x = x

instance ToDynamoDbValue Text where
    toValue = String

instance ToDynamoDbValue a => ToDynamoDbValue [a] where
    toValue = List . fmap toValue

instance ToDynamoDbValue a => ToDynamoDbValue (Maybe a) where
    toValue x = fromMaybe Null $ fmap toValue x

instance ToDynamoDbValue a => ToDynamoDbValue (Togglable a) where
    toValue Togglable {toggled, isOn} =
        Object $
        fromList [("isOn" :: Text, Boolean isOn), ("toggled", toValue toggled)]

instance ToDynamoDbValue Thrusters where
    toValue = Number . fromIntegral . getSpeed

instance ToDynamoDbValue Armor where
    toValue = String . pack . show . getDefenseLevelA

instance ToDynamoDbValue Computer where
    toValue Computer {nodes, bonus} =
        Object $
        fromList
            [ ("nodes", Number $ fromIntegral nodes)
            , ("bonus", Number $ fromIntegral bonus)
            ]

instance ToDynamoDbValue CrewQuarters where
    toValue = String . pack . show

instance ToDynamoDbValue DefensiveCounterMeasures where
    toValue = String . pack . show . getDefenseLevel

instance ToDynamoDbValue DriftEngine where
    toValue = String . pack . show

instance ToDynamoDbValue ExpansionBay where
    toValue = String . pack . show

instance ToDynamoDbValue Sensor where
    toValue Sensor {range, bonus} =
        Object $
        fromList
            [ ("range", String $ pack $ show range)
            , ("bonus", Number $ fromIntegral $ bonus)
            ]

instance ToDynamoDbValue a => ToDynamoDbValue (Arc a) where
    toValue Arc {forward, aft, portSide, starboard} =
        Object $
        fromList
            [ ("forward", toValue forward)
            , ("aft", toValue aft)
            , ("portSide", toValue portSide)
            , ("starboard", toValue starboard)
            ]

instance ToDynamoDbValue (OwnedBy (Build Text Text Text)) where
    toValue (OwnedBy userId build@Build { frame
                                        , powerCoreUnits
                                        , thrusters
                                        , armor
                                        , computer
                                        , crewQuarters
                                        , defensiveCounterMeasures
                                        , driftEngine
                                        , name
                                        , expansionBays
                                        , sensors
                                        , arcWeapons
                                        , turretWeapons
                                        , shields
                                        }) =
        Object $
        fromList
            [ ("HASH1", String userId)
            , ("RANGE1.1", String name)
          --, ("RANGE1.2", Number $ getBuildPoints build) (sorting by build points would be great!  But there is no insance for Build Text Text Text...)
            , ("frame", String frame)
            , ( "powerCoreUnits"
              , Number $ fromIntegral $ getUnits powerCoreUnits)
            , ("thrusters", toValue thrusters)
            , ("armor", toValue armor)
            , ("computer", toValue computer)
            , ("crewQuarters", toValue crewQuarters)
            -- TODO: Countermeasure is a single word!
            , ("defensiveCounterMeasures", toValue defensiveCounterMeasures)
            , ("driftEngine", toValue driftEngine)
            , ("expansionBays", toValue expansionBays)
            , ("sensor", toValue sensors)
            , ("arcWeapons", toValue arcWeapons)
            , ("turretWeapons", toValue turretWeapons)
            , ("shields", toValue shields)
            ]

ownedReferencedBuildToItem ::
       OwnedBy (Build Text Text Text) -> HashMap Text AttributeValue
ownedReferencedBuildToItem x
  -- Invariant, this cannot fail
 =
    let Object y = toValue x
    in fmap toAttrValue y

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
             (fromAttrValue =<< lookup "portside" hashMap) <*>
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

instance FromDynamoDbAttrValue DefensiveCounterMeasures where
    fromAttrValue = (fmap DefensiveCounterMeasures) . fromAttrValue

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

instance FromDynamoDbAttrValue (OwnedBy (Build Text Text Text)) where
    fromAttrValue =
        (\hashMap ->
             OwnedBy <$> (fromAttrValue =<< lookup "userId" hashMap) <*>
             (Build <$> (fromAttrValue =<< lookup "name" hashMap) <*>
              (fromAttrValue =<< lookup "frame" hashMap) <*>
              (fromAttrValue =<< lookup "powerCoreUnits" hashMap) <*>
              (fromAttrValue =<< lookup "thrusters" hashMap) <*>
              (fromAttrValue <$> lookup "armor" hashMap) <*>
              (fromAttrValue =<< lookup "computer" hashMap) <*>
              (fromAttrValue =<< lookup "crewQuarters" hashMap) <*>
              (fromAttrValue <$> lookup "defensiveCounterMeasures" hashMap) <*>
              (fromAttrValue <$> lookup "driftEngine" hashMap) <*>
              (fromAttrValue =<< lookup "expansionBays" hashMap) <*>
              (fromAttrValue =<< lookup "sensor" hashMap) <*>
              (fromAttrValue =<< lookup "arcWeapons" hashMap) <*>
              (fromAttrValue =<< lookup "turretWeapons" hashMap) <*>
              (fromAttrValue =<< lookup "shields" hashMap))) .
        view avM

main :: IO ()
main = return ()
