{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec (describe, it, shouldBe, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Runner (hspec)
import Data.Maybe (fromJust)

import AWS.Lambda.Events.ApiGateway.ProxyResponse
       (ProxyResponse(..), Status(..), ProxyBody(..))
import AWS.Lambda.Events.ApiGateway.ProxyRequest (ProxyRequest)
import BuildController
       (httpAuthorizer, httpBuildServiceErrorHandler,
        httpDynamoBuildRepoErrorHandler, httpHandler,
        httpVersionMismatchHandler, UserId)
import BuildRepo (mockBuildRepoToDynamo, MockState)
import BuildService (buildServiceFromBuildRepo)
import Data.Aeson (decode, Value(..))
import Data.ByteString.Lazy (fromStrict)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, newIORef)
import Data.Map (Map)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as Vector
import Lib
import Polysemy (runM)
import Polysemy.State (runStateIORef)
import Starfinder.Starship.Build (Build, Sensor, CrewQuarters, PowerCoreUnits, Thrusters, Armor, DefensiveCountermeasures, DriftEngine)
import Starfinder.Starship.Computer (Computer)
import Starfinder.Starship.ExpansionBay (ExpansionBay)
import Starfinder.Starship.Togglable (Togglable)
import Starfinder.Starship.Arc (Arc)
import Starfinder.Starship.ReferencedWeapon (ReferencedWeapon)
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
                "should round trip ReferencedWeapon"
                (\(x :: ReferencedWeapon) -> fromAttrValue (toAttrValue x) == Just x)
            prop
                "should round trip Build"
                (\(x :: ETagged (OwnedBy (Build Text ReferencedWeapon Text))) -> fromAttrValue (toAttrValue x) == Just x)
        -- TODO: Need a better strategy here than inline JSON
        describe "Controller behavior" $ do
            it "Should return 403 on an unauthorized GET for a non-existant build" $ do
                app <- makeApp <$> newIORef mempty
                res <- app $ fromJust $ Data.Aeson.decode "{ \"path\": \"/resources/users/me/builds/123\", \"requestContext\": { \"authorizer\": { \"claims\": { \"sub\": \"you\" } }, \"path\": \"resources/users/me/builds/123\", \"accountId\": \"123\", \"resourceId\": \"anId\", \"stage\": \"default\", \"requestId\": \"123\", \"identity\": { \"sourceIp\": \"192.168.0.1\" }, \"resourcePath\": \"/hey\", \"httpMethod\": \"GET\", \"apiId\": \"123\" }, \"resource\": \"someResource\", \"httpMethod\": \"GET\", \"isBase64Encoded\": false, \"body\": \"\" }"
                statusCode (status res) `shouldBe` 403
            it "Should return 404 on an authorized GET for a non-existant build" $ do
                app <- makeApp <$> newIORef mempty
                res <- app $ fromJust $ Data.Aeson.decode "{ \"path\": \"/resources/users/me/builds/123\", \"requestContext\": { \"authorizer\": { \"claims\": { \"sub\": \"me\" } }, \"path\": \"resources/users/me/builds/123\", \"accountId\": \"123\", \"resourceId\": \"anId\", \"stage\": \"default\", \"requestId\": \"123\", \"identity\": { \"sourceIp\": \"192.168.0.1\" }, \"resourcePath\": \"/hey\", \"httpMethod\": \"GET\", \"apiId\": \"123\" }, \"resource\": \"someResource\", \"httpMethod\": \"GET\", \"isBase64Encoded\": false, \"body\": \"\" }"
                statusCode (status res) `shouldBe` 404
            it "Should return 200 on an authorized GET if we create the build first" $ do
                app <- makeApp <$> newIORef mempty
                _ <- app $ fromJust $ Data.Aeson.decode "{ \"path\": \"/resources/users/me/builds\", \"requestContext\": { \"authorizer\": { \"claims\": { \"sub\": \"me\" } }, \"path\": \"resources/users/me/builds\", \"accountId\": \"123\", \"resourceId\": \"anId\", \"stage\": \"default\", \"requestId\": \"123\", \"identity\": { \"sourceIp\": \"192.168.0.1\" }, \"resourcePath\": \"/hey\", \"httpMethod\": \"POST\", \"apiId\": \"123\" }, \"resource\": \"someResource\", \"httpMethod\": \"POST\", \"isBase64Encoded\": false, \"body\": \"{ \\\"name\\\": \\\"123\\\", \\\"frame\\\": \\\"Medium Explorer\\\", \\\"powerCoreUnits\\\": 130, \\\"thrusters\\\": { \\\"isOn\\\": true, \\\"toggled\\\": 12 }, \\\"computer\\\": { \\\"isOn\\\": true, \\\"toggled\\\": { \\\"bonus\\\": 0, \\\"nodes\\\": 0 } }, \\\"crewQuarters\\\": \\\"Common\\\", \\\"expansionBays\\\": [], \\\"sensors\\\": { \\\"range\\\": \\\"Medium\\\", \\\"bonus\\\": 0 }, \\\"arcWeapons\\\": { \\\"forward\\\": [], \\\"portSide\\\": [], \\\"starboard\\\": [], \\\"aft\\\": [] }, \\\"turretWeapons\\\": [], \\\"shields\\\": { \\\"isOn\\\": true, \\\"toggled\\\": \\\"Light Shields 50\\\" } }\" }"
                res <- app $ fromJust $ Data.Aeson.decode "{ \"path\": \"/resources/users/me/builds/123\", \"requestContext\": { \"authorizer\": { \"claims\": { \"sub\": \"me\" } }, \"path\": \"resources/users/me/builds/123\", \"accountId\": \"123\", \"resourceId\": \"anId\", \"stage\": \"default\", \"requestId\": \"123\", \"identity\": { \"sourceIp\": \"192.168.0.1\" }, \"resourcePath\": \"/hey\", \"httpMethod\": \"GET\", \"apiId\": \"123\" }, \"resource\": \"someResource\", \"httpMethod\": \"GET\", \"isBase64Encoded\": false, \"body\": \"\" }"
                statusCode (status res) `shouldBe` 200
            it "Should return 200 on an authorized _list_ GET if we create the build first" $ do
                app <- makeApp <$> newIORef mempty
                _ <- app $ fromJust $ Data.Aeson.decode "{ \"path\": \"/resources/users/abc/builds\", \"requestContext\": { \"authorizer\": { \"claims\": { \"sub\": \"abc\" } }, \"path\": \"resources/users/abc/builds\", \"accountId\": \"123\", \"resourceId\": \"anId\", \"stage\": \"default\", \"requestId\": \"123\", \"identity\": { \"sourceIp\": \"192.168.0.1\" }, \"resourcePath\": \"/hey\", \"httpMethod\": \"POST\", \"apiId\": \"123\" }, \"resource\": \"someResource\", \"httpMethod\": \"POST\", \"isBase64Encoded\": false, \"body\": \"{ \\\"name\\\": \\\"123\\\", \\\"frame\\\": \\\"Medium Explorer\\\", \\\"powerCoreUnits\\\": 130, \\\"thrusters\\\": { \\\"isOn\\\": true, \\\"toggled\\\": 12 }, \\\"computer\\\": { \\\"isOn\\\": true, \\\"toggled\\\": { \\\"bonus\\\": 0, \\\"nodes\\\": 0 } }, \\\"crewQuarters\\\": \\\"Common\\\", \\\"expansionBays\\\": [], \\\"sensors\\\": { \\\"range\\\": \\\"Medium\\\", \\\"bonus\\\": 0 }, \\\"arcWeapons\\\": { \\\"forward\\\": [], \\\"portSide\\\": [], \\\"starboard\\\": [], \\\"aft\\\": [] }, \\\"turretWeapons\\\": [], \\\"shields\\\": { \\\"isOn\\\": true, \\\"toggled\\\": \\\"Light Shields 50\\\" } }\" }"
                _ <- app $ fromJust $ Data.Aeson.decode "{ \"path\": \"/resources/users/abc/builds\", \"requestContext\": { \"authorizer\": { \"claims\": { \"sub\": \"abc\" } }, \"path\": \"resources/users/abc/builds\", \"accountId\": \"123\", \"resourceId\": \"anId\", \"stage\": \"default\", \"requestId\": \"123\", \"identity\": { \"sourceIp\": \"192.168.0.1\" }, \"resourcePath\": \"/hey\", \"httpMethod\": \"POST\", \"apiId\": \"123\" }, \"resource\": \"someResource\", \"httpMethod\": \"POST\", \"isBase64Encoded\": false, \"body\": \"{ \\\"name\\\": \\\"456\\\", \\\"frame\\\": \\\"Medium Explorer\\\", \\\"powerCoreUnits\\\": 130, \\\"thrusters\\\": { \\\"isOn\\\": true, \\\"toggled\\\": 12 }, \\\"computer\\\": { \\\"isOn\\\": true, \\\"toggled\\\": { \\\"bonus\\\": 0, \\\"nodes\\\": 0 } }, \\\"crewQuarters\\\": \\\"Common\\\", \\\"expansionBays\\\": [], \\\"sensors\\\": { \\\"range\\\": \\\"Medium\\\", \\\"bonus\\\": 0 }, \\\"arcWeapons\\\": { \\\"forward\\\": [], \\\"portSide\\\": [], \\\"starboard\\\": [], \\\"aft\\\": [] }, \\\"turretWeapons\\\": [], \\\"shields\\\": { \\\"isOn\\\": true, \\\"toggled\\\": \\\"Light Shields 50\\\" } }\" }"
                res <- app $ fromJust $ Data.Aeson.decode "{ \"path\": \"/resources/users/abc/builds\", \"requestContext\": { \"authorizer\": { \"claims\": { \"sub\": \"abc\" } }, \"path\": \"resources/users/abc/builds\", \"accountId\": \"123\", \"resourceId\": \"anId\", \"stage\": \"default\", \"requestId\": \"123\", \"identity\": { \"sourceIp\": \"192.168.0.1\" }, \"resourcePath\": \"/hey\", \"httpMethod\": \"GET\", \"apiId\": \"123\" }, \"resource\": \"someResource\", \"httpMethod\": \"GET\", \"isBase64Encoded\": false, \"body\": \"\" }"
                statusCode (status res) `shouldBe` 200
                let bodyValue = decode (fromStrict (encodeUtf8 (serialized (body res))))
                let expected =
                        list
                            [ obj
                                  [ ("name", String "123")
                                  , ( "link"
                                    , String "/resources/users/abc/builds/123")
                                  ]
                            , obj
                                  [ ("name", String "456")
                                  , ( "link"
                                    , String "/resources/users/abc/builds/456")
                                  ]
                            ]
                bodyValue `shouldBe` Just expected
            it "Should return 200 on an authorized _list_ GET if we create the build first, but be empty if it's not our builds" $ do
                app <- makeApp <$> newIORef mempty
                _ <- app $ fromJust $ Data.Aeson.decode "{ \"path\": \"/resources/users/abc/builds\", \"requestContext\": { \"authorizer\": { \"claims\": { \"sub\": \"abc\" } }, \"path\": \"resources/users/abc/builds\", \"accountId\": \"123\", \"resourceId\": \"anId\", \"stage\": \"default\", \"requestId\": \"123\", \"identity\": { \"sourceIp\": \"192.168.0.1\" }, \"resourcePath\": \"/hey\", \"httpMethod\": \"POST\", \"apiId\": \"123\" }, \"resource\": \"someResource\", \"httpMethod\": \"POST\", \"isBase64Encoded\": false, \"body\": \"{ \\\"name\\\": \\\"123\\\", \\\"frame\\\": \\\"Medium Explorer\\\", \\\"powerCoreUnits\\\": 130, \\\"thrusters\\\": { \\\"isOn\\\": true, \\\"toggled\\\": 12 }, \\\"computer\\\": { \\\"isOn\\\": true, \\\"toggled\\\": { \\\"bonus\\\": 0, \\\"nodes\\\": 0 } }, \\\"crewQuarters\\\": \\\"Common\\\", \\\"expansionBays\\\": [], \\\"sensors\\\": { \\\"range\\\": \\\"Medium\\\", \\\"bonus\\\": 0 }, \\\"arcWeapons\\\": { \\\"forward\\\": [], \\\"portSide\\\": [], \\\"starboard\\\": [], \\\"aft\\\": [] }, \\\"turretWeapons\\\": [], \\\"shields\\\": { \\\"isOn\\\": true, \\\"toggled\\\": \\\"Light Shields 50\\\" } }\" }"
                _ <- app $ fromJust $ Data.Aeson.decode "{ \"path\": \"/resources/users/abc/builds\", \"requestContext\": { \"authorizer\": { \"claims\": { \"sub\": \"abc\" } }, \"path\": \"resources/users/abc/builds\", \"accountId\": \"123\", \"resourceId\": \"anId\", \"stage\": \"default\", \"requestId\": \"123\", \"identity\": { \"sourceIp\": \"192.168.0.1\" }, \"resourcePath\": \"/hey\", \"httpMethod\": \"POST\", \"apiId\": \"123\" }, \"resource\": \"someResource\", \"httpMethod\": \"POST\", \"isBase64Encoded\": false, \"body\": \"{ \\\"name\\\": \\\"456\\\", \\\"frame\\\": \\\"Medium Explorer\\\", \\\"powerCoreUnits\\\": 130, \\\"thrusters\\\": { \\\"isOn\\\": true, \\\"toggled\\\": 12 }, \\\"computer\\\": { \\\"isOn\\\": true, \\\"toggled\\\": { \\\"bonus\\\": 0, \\\"nodes\\\": 0 } }, \\\"crewQuarters\\\": \\\"Common\\\", \\\"expansionBays\\\": [], \\\"sensors\\\": { \\\"range\\\": \\\"Medium\\\", \\\"bonus\\\": 0 }, \\\"arcWeapons\\\": { \\\"forward\\\": [], \\\"portSide\\\": [], \\\"starboard\\\": [], \\\"aft\\\": [] }, \\\"turretWeapons\\\": [], \\\"shields\\\": { \\\"isOn\\\": true, \\\"toggled\\\": \\\"Light Shields 50\\\" } }\" }"
                res <- app $ fromJust $ Data.Aeson.decode "{ \"path\": \"/resources/users/abd/builds\", \"requestContext\": { \"authorizer\": { \"claims\": { \"sub\": \"abd\" } }, \"path\": \"resources/users/abd/builds\", \"accountId\": \"123\", \"resourceId\": \"anId\", \"stage\": \"default\", \"requestId\": \"123\", \"identity\": { \"sourceIp\": \"192.168.0.1\" }, \"resourcePath\": \"/hey\", \"httpMethod\": \"GET\", \"apiId\": \"123\" }, \"resource\": \"someResource\", \"httpMethod\": \"GET\", \"isBase64Encoded\": false, \"body\": \"\" }"
                statusCode (status res) `shouldBe` 200
                let bodyValue = decode (fromStrict (encodeUtf8 (serialized (body res))))
                bodyValue `shouldBe` Just (list [])
            it "Should return 409 if we try to POST a build twice" $ do
                app <- makeApp <$> newIORef mempty
                first <- app $ fromJust $ Data.Aeson.decode "{ \"path\": \"/resources/users/me/builds\", \"requestContext\": { \"authorizer\": { \"claims\": { \"sub\": \"me\" } }, \"path\": \"resources/users/me/builds\", \"accountId\": \"123\", \"resourceId\": \"anId\", \"stage\": \"default\", \"requestId\": \"123\", \"identity\": { \"sourceIp\": \"192.168.0.1\" }, \"resourcePath\": \"/hey\", \"httpMethod\": \"POST\", \"apiId\": \"123\" }, \"resource\": \"someResource\", \"httpMethod\": \"POST\", \"isBase64Encoded\": false, \"body\": \"{ \\\"name\\\": \\\"123\\\", \\\"frame\\\": \\\"Medium Explorer\\\", \\\"powerCoreUnits\\\": 130, \\\"thrusters\\\": { \\\"isOn\\\": true, \\\"toggled\\\": 12 }, \\\"computer\\\": { \\\"isOn\\\": true, \\\"toggled\\\": { \\\"bonus\\\": 0, \\\"nodes\\\": 0 } }, \\\"crewQuarters\\\": \\\"Common\\\", \\\"expansionBays\\\": [], \\\"sensors\\\": { \\\"range\\\": \\\"Medium\\\", \\\"bonus\\\": 0 }, \\\"arcWeapons\\\": { \\\"forward\\\": [], \\\"portSide\\\": [], \\\"starboard\\\": [], \\\"aft\\\": [] }, \\\"turretWeapons\\\": [], \\\"shields\\\": { \\\"isOn\\\": true, \\\"toggled\\\": \\\"Light Shields 50\\\" } }\" }"
                second <- app $ fromJust $ Data.Aeson.decode "{ \"path\": \"/resources/users/me/builds\", \"requestContext\": { \"authorizer\": { \"claims\": { \"sub\": \"me\" } }, \"path\": \"resources/users/me/builds\", \"accountId\": \"123\", \"resourceId\": \"anId\", \"stage\": \"default\", \"requestId\": \"123\", \"identity\": { \"sourceIp\": \"192.168.0.1\" }, \"resourcePath\": \"/hey\", \"httpMethod\": \"POST\", \"apiId\": \"123\" }, \"resource\": \"someResource\", \"httpMethod\": \"POST\", \"isBase64Encoded\": false, \"body\": \"{ \\\"name\\\": \\\"123\\\", \\\"frame\\\": \\\"Medium Explorer\\\", \\\"powerCoreUnits\\\": 130, \\\"thrusters\\\": { \\\"isOn\\\": true, \\\"toggled\\\": 12 }, \\\"computer\\\": { \\\"isOn\\\": true, \\\"toggled\\\": { \\\"bonus\\\": 0, \\\"nodes\\\": 0 } }, \\\"crewQuarters\\\": \\\"Common\\\", \\\"expansionBays\\\": [], \\\"sensors\\\": { \\\"range\\\": \\\"Medium\\\", \\\"bonus\\\": 0 }, \\\"arcWeapons\\\": { \\\"forward\\\": [], \\\"portSide\\\": [], \\\"starboard\\\": [], \\\"aft\\\": [] }, \\\"turretWeapons\\\": [], \\\"shields\\\": { \\\"isOn\\\": true, \\\"toggled\\\": \\\"Light Shields 50\\\" } }\" }"
                statusCode (status first) `shouldBe` 200
                statusCode (status second) `shouldBe` 409
            it "Should return 412 if we try to PUT to a build with the wrong ETag" $ do
                app <- makeApp <$> newIORef mempty
                first <- app $ fromJust $ Data.Aeson.decode "{ \"path\": \"/resources/users/me/builds\", \"requestContext\": { \"authorizer\": { \"claims\": { \"sub\": \"me\" } }, \"path\": \"resources/users/me/builds\", \"accountId\": \"123\", \"resourceId\": \"anId\", \"stage\": \"default\", \"requestId\": \"123\", \"identity\": { \"sourceIp\": \"192.168.0.1\" }, \"resourcePath\": \"/hey\", \"httpMethod\": \"POST\", \"apiId\": \"123\" }, \"resource\": \"someResource\", \"httpMethod\": \"POST\", \"isBase64Encoded\": false, \"body\": \"{ \\\"name\\\": \\\"123\\\", \\\"frame\\\": \\\"Medium Explorer\\\", \\\"powerCoreUnits\\\": 130, \\\"thrusters\\\": { \\\"isOn\\\": true, \\\"toggled\\\": 12 }, \\\"computer\\\": { \\\"isOn\\\": true, \\\"toggled\\\": { \\\"bonus\\\": 0, \\\"nodes\\\": 0 } }, \\\"crewQuarters\\\": \\\"Common\\\", \\\"expansionBays\\\": [], \\\"sensors\\\": { \\\"range\\\": \\\"Medium\\\", \\\"bonus\\\": 0 }, \\\"arcWeapons\\\": { \\\"forward\\\": [], \\\"portSide\\\": [], \\\"starboard\\\": [], \\\"aft\\\": [] }, \\\"turretWeapons\\\": [], \\\"shields\\\": { \\\"isOn\\\": true, \\\"toggled\\\": \\\"Light Shields 50\\\" } }\" }"
                second <- app $ fromJust $ Data.Aeson.decode "{ \"path\": \"/resources/users/me/builds/123\", \"requestContext\": { \"authorizer\": { \"claims\": { \"sub\": \"me\" } }, \"path\": \"resources/users/me/builds/123\", \"accountId\": \"123\", \"resourceId\": \"anId\", \"stage\": \"default\", \"requestId\": \"123\", \"identity\": { \"sourceIp\": \"192.168.0.1\" }, \"resourcePath\": \"/hey\", \"httpMethod\": \"PUT\", \"apiId\": \"123\" }, \"resource\": \"someResource\", \"httpMethod\": \"PUT\", \"headers\": { \"If-Match\": \"\\\"123\\\"\" }, \"multiValueHeaders\": { \"If-Match\": [\"123\"] }, \"isBase64Encoded\": false, \"body\": \"{ \\\"name\\\": \\\"123\\\", \\\"frame\\\": \\\"Medium Explorer\\\", \\\"powerCoreUnits\\\": 130, \\\"thrusters\\\": { \\\"isOn\\\": true, \\\"toggled\\\": 11 }, \\\"computer\\\": { \\\"isOn\\\": true, \\\"toggled\\\": { \\\"bonus\\\": 0, \\\"nodes\\\": 0 } }, \\\"crewQuarters\\\": \\\"Common\\\", \\\"expansionBays\\\": [], \\\"sensors\\\": { \\\"range\\\": \\\"Medium\\\", \\\"bonus\\\": 0 }, \\\"arcWeapons\\\": { \\\"forward\\\": [], \\\"portSide\\\": [], \\\"starboard\\\": [], \\\"aft\\\": [] }, \\\"turretWeapons\\\": [], \\\"shields\\\": { \\\"isOn\\\": true, \\\"toggled\\\": \\\"Light Shields 50\\\" } }\" }"
                statusCode (status first) `shouldBe` 200
                statusCode (status second) `shouldBe` 412
            it "Should return update if we PUT to a build with the correct ETag" $ do
                app <- makeApp <$> newIORef mempty
                first <- app $ fromJust $ Data.Aeson.decode "{ \"path\": \"/resources/users/me/builds\", \"requestContext\": { \"authorizer\": { \"claims\": { \"sub\": \"me\" } }, \"path\": \"resources/users/me/builds\", \"accountId\": \"123\", \"resourceId\": \"anId\", \"stage\": \"default\", \"requestId\": \"123\", \"identity\": { \"sourceIp\": \"192.168.0.1\" }, \"resourcePath\": \"/hey\", \"httpMethod\": \"POST\", \"apiId\": \"123\" }, \"resource\": \"someResource\", \"httpMethod\": \"POST\", \"isBase64Encoded\": false, \"body\": \"{ \\\"name\\\": \\\"123\\\", \\\"frame\\\": \\\"Medium Explorer\\\", \\\"powerCoreUnits\\\": 130, \\\"thrusters\\\": { \\\"isOn\\\": true, \\\"toggled\\\": 12 }, \\\"computer\\\": { \\\"isOn\\\": true, \\\"toggled\\\": { \\\"bonus\\\": 0, \\\"nodes\\\": 0 } }, \\\"crewQuarters\\\": \\\"Common\\\", \\\"expansionBays\\\": [], \\\"sensors\\\": { \\\"range\\\": \\\"Medium\\\", \\\"bonus\\\": 0 }, \\\"arcWeapons\\\": { \\\"forward\\\": [], \\\"portSide\\\": [], \\\"starboard\\\": [], \\\"aft\\\": [] }, \\\"turretWeapons\\\": [], \\\"shields\\\": { \\\"isOn\\\": true, \\\"toggled\\\": \\\"Light Shields 50\\\" } }\" }"
                -- TODO: use the ETag
                second <- app $ fromJust $ Data.Aeson.decode "{ \"path\": \"/resources/users/me/builds/123\", \"requestContext\": { \"authorizer\": { \"claims\": { \"sub\": \"me\" } }, \"path\": \"resources/users/me/builds/123\", \"accountId\": \"123\", \"resourceId\": \"anId\", \"stage\": \"default\", \"requestId\": \"123\", \"identity\": { \"sourceIp\": \"192.168.0.1\" }, \"resourcePath\": \"/hey\", \"httpMethod\": \"PUT\", \"apiId\": \"123\" }, \"resource\": \"someResource\", \"httpMethod\": \"PUT\", \"headers\": { \"If-Match\": \"\\\"-1266259270271793073\\\"\" }, \"multiValueHeaders\": { \"If-Match\": [\"123\"] }, \"isBase64Encoded\": false, \"body\": \"{ \\\"name\\\": \\\"123\\\", \\\"frame\\\": \\\"Medium Explorer\\\", \\\"powerCoreUnits\\\": 130, \\\"thrusters\\\": { \\\"isOn\\\": true, \\\"toggled\\\": 11 }, \\\"computer\\\": { \\\"isOn\\\": true, \\\"toggled\\\": { \\\"bonus\\\": 0, \\\"nodes\\\": 0 } }, \\\"crewQuarters\\\": \\\"Common\\\", \\\"expansionBays\\\": [], \\\"sensors\\\": { \\\"range\\\": \\\"Medium\\\", \\\"bonus\\\": 0 }, \\\"arcWeapons\\\": { \\\"forward\\\": [], \\\"portSide\\\": [], \\\"starboard\\\": [], \\\"aft\\\": [] }, \\\"turretWeapons\\\": [], \\\"shields\\\": { \\\"isOn\\\": true, \\\"toggled\\\": \\\"Light Shields 50\\\" } }\" }"
                statusCode (status first) `shouldBe` 200
                statusCode (status second) `shouldBe` 200

makeApp :: IORef MockState -> ProxyRequest UserId -> IO ProxyResponse
makeApp ioRef =
    runM .
    httpAuthorizer .
    httpVersionMismatchHandler .
    httpDynamoBuildRepoErrorHandler .
    runStateIORef ioRef .
    mockBuildRepoToDynamo .
    httpBuildServiceErrorHandler . buildServiceFromBuildRepo . httpHandler

list :: [Value] -> Value
list = Array . Vector.fromList

obj :: [(Text, Value)] -> Value
obj = Object . HashMap.fromList
