module Tests exposing (..)

import Test exposing (..)
import Expect
import Starship exposing (..)
import ShipAssets exposing (..)


all : Test
all =
    describe "Starship"
        [ describe "Build Points"
            [ test "Norikama Dropship" <|
                \() ->
                    Expect.equal 201 (getStarshipBuildPoints norikamaDropship)
            , test "Blackwind Sepulcher" <|
                \() ->
                    Expect.equal 137 (getStarshipBuildPoints blackwindSepulcher)
            ]
        , describe "Build Validation"
            [ test "Norikama Dropship" <|
                \() ->
                    Expect.equal [] (validateStarship norikamaDropship)
            , test "Blackwind Sepulcher" <|
                \() ->
                    Expect.equal [] (validateStarship blackwindSepulcher)
            ]
        , describe "Max Hit Points"
            [ test "Norikama Dropship" <|
                \() ->
                    Expect.equal 100 (getMaxHitPoints norikamaDropship)
            , test "Blackwind Sepulcher" <|
                \() ->
                    Expect.equal 85 (getMaxHitPoints blackwindSepulcher)
            ]
        ]
