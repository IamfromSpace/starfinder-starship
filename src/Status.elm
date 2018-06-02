module Status exposing (..)

import Arc exposing (Arc)


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


type AnArc
    = Forward
    | Aft
    | Port
    | Starboard


type PatchableSystem
    = LifeSupport
    | Sensors
    | WeaponsArray AnArc
    | Engines
    | PowerCore


patchWeapon : AnArc -> Arc (Maybe CriticalStatus) -> Arc (Maybe CriticalStatus)
patchWeapon arc weaponsArray =
    case arc of
        Forward ->
            { weaponsArray | forward = Maybe.andThen patchCriticalStatus weaponsArray.forward }

        Aft ->
            { weaponsArray | aft = Maybe.andThen patchCriticalStatus weaponsArray.aft }

        Port ->
            { weaponsArray | portSide = Maybe.andThen patchCriticalStatus weaponsArray.portSide }

        Starboard ->
            { weaponsArray | starboard = Maybe.andThen patchCriticalStatus weaponsArray.starboard }


patchStatus : PatchableSystem -> Status -> Status
patchStatus system status =
    case system of
        LifeSupport ->
            { status | lifeSupport = Maybe.andThen patchCriticalStatus status.lifeSupport }

        Sensors ->
            { status | sensors = Maybe.andThen patchCriticalStatus status.sensors }

        WeaponsArray arc ->
            { status | weaponsArray = patchWeapon arc status.weaponsArray }

        Engines ->
            { status | engines = Maybe.andThen patchCriticalStatus status.engines }

        PowerCore ->
            { status | powerCore = Maybe.andThen patchCriticalStatus status.powerCore }
