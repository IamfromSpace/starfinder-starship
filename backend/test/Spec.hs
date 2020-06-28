{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Runner (hspec)
import Data.Maybe (fromJust)

import Lib
import Starfinder.Starship.Build (Build, Sensor, CrewQuarters, PowerCoreUnits, Thrusters, Armor, DefensiveCountermeasures, DriftEngine)
import Starfinder.Starship.Computer (Computer)
import Starfinder.Starship.ExpansionBay (ExpansionBay)
import Starfinder.Starship.Togglable (Togglable)
import Starfinder.Starship.Arc (Arc)
import Data.Text

main :: IO ()
main =
    hspec $ do
        describe "From and To DynamoDB AttributeValue" $ do
            prop
                "should round trip Computer"
                (\(x :: Computer) -> fromAttrValue (toAttrValue x) == Just x)
            prop
                "should round trip Togglable"
                (\(x :: Togglable Int) -> fromAttrValue (toAttrValue x) == Just x)
            prop
                "should round trip Arc"
                (\(x :: Arc Int) -> fromAttrValue (toAttrValue x) == Just x)
            prop
                "should round trip Sensor"
                (\(x :: Sensor) -> fromAttrValue (toAttrValue x) == Just x)
            prop
                "should round trip CrewQuarters"
                (\(x :: CrewQuarters) -> fromAttrValue (toAttrValue x) == Just x)
            prop
                "should round trip PowerCoreUnits"
                (\(x :: PowerCoreUnits) -> fromAttrValue (toAttrValue x) == Just x)
            prop
                "should round trip Thrusters"
                (\(x :: Thrusters) -> fromAttrValue (toAttrValue x) == Just x)
            prop
                "should round trip Armor"
                (\(x :: Armor) -> fromAttrValue (toAttrValue x) == Just x)
            prop
                "should round trip DefensiveCountermeasures"
                (\(x :: DefensiveCountermeasures) -> fromAttrValue (toAttrValue x) == Just x)
            prop
                "should round trip DriftEngine"
                (\(x :: DriftEngine) -> fromAttrValue (toAttrValue x) == Just x)
            prop
                "should round trip ExpansionBay"
                (\(x :: ExpansionBay) -> fromAttrValue (toAttrValue x) == Just x)
            prop
                "should round trip Build"
                (\(x :: ETagged (OwnedBy (Build Text Text Text))) -> fromAttrValue (toAttrValue x) == Just x)
