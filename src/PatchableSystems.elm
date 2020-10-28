module PatchableSystems exposing (PatchableSystem(..), PatchableSystems, getPatchableSystem, pickPatchableSystem, pure, updatePatchableSystem)

import Arc exposing (AnArc, Arc)
import Random exposing (Generator)


type alias PatchableSystems a =
    { lifeSupport : a
    , sensors : a
    , weaponsArray : Arc a
    , engines : a
    , powerCore : a
    }


pure : a -> PatchableSystems a
pure a =
    { lifeSupport = a
    , sensors = a
    , weaponsArray = Arc.pure a
    , engines = a
    , powerCore = a
    }


type PatchableSystem
    = LifeSupport
    | Sensors
    | WeaponsArray AnArc
    | Engines
    | PowerCore



-- A generator that picks a system to damage based on the odds
-- of impacting that system.


pickPatchableSystem : Generator PatchableSystem
pickPatchableSystem =
    Random.weighted ( 10, LifeSupport )
        [ ( 20, Sensors )
        , ( 7.5, WeaponsArray Arc.Forward )
        , ( 7.5, WeaponsArray Arc.Aft )
        , ( 7.5, WeaponsArray Arc.Port )
        , ( 7.5, WeaponsArray Arc.Starboard )
        , ( 20, Engines )
        , ( 20, PowerCore )
        ]


getPatchableSystem : PatchableSystem -> PatchableSystems a -> a
getPatchableSystem system =
    case system of
        LifeSupport ->
            .lifeSupport

        Sensors ->
            .sensors

        WeaponsArray arc ->
            .weaponsArray >> Arc.getArc arc

        Engines ->
            .engines

        PowerCore ->
            .powerCore


updatePatchableSystem : (a -> a) -> PatchableSystem -> PatchableSystems a -> PatchableSystems a
updatePatchableSystem fn system systems =
    case system of
        LifeSupport ->
            { systems | lifeSupport = fn systems.lifeSupport }

        Sensors ->
            { systems | sensors = fn systems.sensors }

        WeaponsArray arc ->
            { systems | weaponsArray = Arc.updateArc fn arc systems.weaponsArray }

        Engines ->
            { systems | engines = fn systems.engines }

        PowerCore ->
            { systems | powerCore = fn systems.powerCore }
