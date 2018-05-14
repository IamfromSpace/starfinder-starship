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



-- TODO: Tediously do the rest (8 total), or find a better way
-- TODO: Is it worth making `type PatchableSystems = Sensors | ForwardArc | ...`?


patchLifeSupport : Status -> Status
patchLifeSupport status =
    { status | lifeSupport = Maybe.andThen patchCriticalStatus status.lifeSupport }
