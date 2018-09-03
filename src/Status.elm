module Status exposing (CriticalStatus, PatchableSystem(..), Severity(..), Status, damage, damageArc, damageSeverity, damageStatus, getEffectiveCriticalStatus, patchCriticalStatus, patchSeverity, patchStatus, pickPatchableSystem, tick, tickCriticalStatus, updateCriticalStatus)

import Arc exposing (AnArc, Arc)
import Random exposing (Generator)
import Starship exposing (Starship)
import Switch exposing (Switch(..))
import Togglable exposing (meta)


type Severity
    = Glitching
    | Malfunctioning
    | Wrecked


type alias CriticalStatus =
    { severity : Severity
    , remainingRounds :
        -- If Nothing, the effect last until repaired
        Maybe Int
    , heldTogether :
        -- Holding together reduces two levels of severity for one round
        Bool
    , quickFixed :
        -- QuickFix ignores this affect for 1 hour (the duration of combat)
        Bool
    }


type alias Status =
    { damage : Int
    , shields : Arc Int
    , lifeSupport : Maybe CriticalStatus
    , sensors : Maybe CriticalStatus
    , weaponsArray : Arc (Maybe CriticalStatus)
    , engines : Maybe CriticalStatus
    , powerCore : Maybe CriticalStatus
    }


getEffectiveCriticalStatus : CriticalStatus -> Maybe Severity
getEffectiveCriticalStatus cs =
    if cs.quickFixed then
        Nothing

    else if cs.heldTogether then
        patchSeverity cs.severity
            |> Maybe.andThen patchSeverity

    else
        Just cs.severity


patchSeverity : Severity -> Maybe Severity
patchSeverity severity =
    case severity of
        Glitching ->
            Nothing

        Malfunctioning ->
            Just Glitching

        Wrecked ->
            Just Malfunctioning


patchCriticalStatus : CriticalStatus -> Maybe CriticalStatus
patchCriticalStatus criticalStatus =
    Maybe.map
        (\newSeverity -> { criticalStatus | severity = newSeverity })
        (patchSeverity criticalStatus.severity)


damageSeverity : Severity -> Severity
damageSeverity severity =
    case severity of
        Glitching ->
            Malfunctioning

        Malfunctioning ->
            Wrecked

        Wrecked ->
            Wrecked


damage : Maybe Int -> Maybe CriticalStatus -> CriticalStatus
damage rounds mCriticalStatus =
    case mCriticalStatus of
        Just criticalStatus ->
            case rounds of
                Just _ ->
                    criticalStatus

                Nothing ->
                    { criticalStatus | severity = damageSeverity criticalStatus.severity }

        Nothing ->
            { severity = Glitching
            , remainingRounds = rounds
            , heldTogether = False
            , quickFixed = False
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


updateCriticalStatus : (Maybe CriticalStatus -> Maybe CriticalStatus) -> PatchableSystem -> Status -> Status
updateCriticalStatus fn system status =
    case system of
        LifeSupport ->
            { status | lifeSupport = fn status.lifeSupport }

        Sensors ->
            { status | sensors = fn status.sensors }

        WeaponsArray arc ->
            { status | weaponsArray = Arc.updateArc fn arc status.weaponsArray }

        Engines ->
            { status | engines = fn status.engines }

        PowerCore ->
            { status | powerCore = fn status.powerCore }


patchStatus : PatchableSystem -> Status -> Status
patchStatus =
    updateCriticalStatus (Maybe.andThen patchCriticalStatus)


damageStatus : Maybe Int -> PatchableSystem -> Status -> Status
damageStatus rounds =
    updateCriticalStatus (damage rounds >> Just)


tickCriticalStatus : CriticalStatus -> Maybe CriticalStatus
tickCriticalStatus cs =
    let
        unheld =
            { cs | heldTogether = False }
    in
    case unheld.remainingRounds of
        Just x ->
            if x > 1 then
                Just { unheld | remainingRounds = Just (x - 1) }

            else
                Nothing

        Nothing ->
            Just unheld


tick : Status -> Status
tick status =
    let
        update =
            updateCriticalStatus (Maybe.andThen tickCriticalStatus)
    in
    status
        |> update LifeSupport
        |> update Sensors
        |> update (WeaponsArray Arc.Forward)
        |> update (WeaponsArray Arc.Aft)
        |> update (WeaponsArray Arc.Starboard)
        |> update (WeaponsArray Arc.Port)
        |> update Engines
        |> update PowerCore


damageArc : PatchableSystem -> Starship -> AnArc -> Int -> Status -> Status
damageArc criticalSystem build arc amount status =
    let
        criticalThreshold =
            build.frame.criticalThreshold

        shieldsOn =
            meta build.shields == On

        shielding =
            if shieldsOn then
                Arc.getArc arc status.shields

            else
                0

        hullDamage =
            amount - shielding
    in
    if hullDamage <= 0 then
        { status | shields = Arc.updateArc (\x -> x - amount) arc status.shields }

    else
        let
            nonCritical =
                { status
                    | shields = Arc.updateArc (always 0) arc status.shields
                    , damage = status.damage + hullDamage
                }
        in
        if hullDamage >= criticalThreshold then
            damageStatus Nothing criticalSystem nonCritical

        else
            nonCritical
