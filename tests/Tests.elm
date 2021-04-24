module Tests exposing (all)

-- Main is imported to ensure everything compiles

import Expect
import Main
import ShipAssets exposing (..)
import Starship exposing (..)
import Test exposing (..)


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
