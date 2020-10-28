module CriticalStatus exposing (CriticalStatus, PatchEffectiveness(..), Severity(..), basePatchDC, damage, getEffectiveSeverity, holdItTogether, patch, patchCount, patchProgress, quickFix)


type Severity
    = Glitching
    | Malfunctioning
    | Wrecked



-- Patches form a sort of Paneo numeral chain, where the stack is the number of
-- patches required to move to the next.  So W (M (M (G R))) represents a still
-- Wrecked condition, but with only one remaining patch left to move to
-- Malfunctioning.  This is helpful because engineers can take actions that
-- apply multiple patches, but only to the same level of condition.


type Patches
    = W Patches
    | M Patches
    | G Patches
    | R


type PatchEffectiveness
    = Single
    | Double
    | Triple


type CausalSeverity
    = Emp Int
    | CriticalDamage Severity


type alias CriticalStatus =
    { severity : CausalSeverity
    , patches : Patches
    , heldTogether :
        -- Holding together reduces two levels of severity for one round
        Maybe Int
    , quickFixed :
        -- QuickFix ignores this affect for 1 hour (the duration of combat)
        Bool
    }


considerEmp : Int -> CausalSeverity -> Maybe Severity
considerEmp currentRound causalSeverity =
    case causalSeverity of
        Emp untilRound ->
            if currentRound <= untilRound then
                Just Glitching

            else
                Nothing

        CriticalDamage severity ->
            Just severity


patchCount_ : Patches -> Int
patchCount_ patches =
    case patches of
        W (W (W x)) ->
            0

        W (W (M x)) ->
            1

        W (M x) ->
            2

        M (M x) ->
            0

        M (G x) ->
            1

        G R ->
            0

        _ ->
            0


patchCount : CriticalStatus -> Int
patchCount =
    patchCount_ << .patches


patchProgress : Patches -> ( Int, Int )
patchProgress patches =
    case patches of
        W (W (W x)) ->
            ( 3, 3 )

        W (W (M x)) ->
            ( 2, 3 )

        W (M x) ->
            ( 1, 3 )

        M (M x) ->
            ( 2, 2 )

        M (G x) ->
            ( 1, 2 )

        G R ->
            ( 0, 1 )

        _ ->
            ( 0, 0 )


applyPatches : Patches -> Maybe Severity
applyPatches patches =
    case patches of
        W _ ->
            Just Wrecked

        M _ ->
            Just Malfunctioning

        G _ ->
            Just Glitching

        R ->
            Nothing


applyHoldTogether : Int -> Maybe Int -> Severity -> Maybe Severity
applyHoldTogether currentRound heldTogether severity =
    case ( heldTogether, severity ) of
        ( Just onRound, Wrecked ) ->
            if onRound == currentRound then
                Just Glitching

            else
                Just severity

        ( Just onRound, _ ) ->
            if onRound == currentRound then
                Nothing

            else
                Just severity

        ( Nothing, _ ) ->
            Just severity


getEffectiveSeverity : Int -> CriticalStatus -> Maybe Severity
getEffectiveSeverity currentRound cs =
    if cs.quickFixed then
        Nothing

    else
        considerEmp currentRound cs.severity
            |> Maybe.andThen (always (applyPatches cs.patches))
            |> Maybe.andThen (applyHoldTogether currentRound cs.heldTogether)


basePatchDCPatches : PatchEffectiveness -> Patches -> Maybe Int
basePatchDCPatches pe patches =
    case ( pe, patches ) of
        ( Single, G _ ) ->
            Just 10

        ( Single, M _ ) ->
            Just 15

        ( Double, M (M _) ) ->
            Just 20

        ( Single, W _ ) ->
            Just 20

        ( Double, W (W _) ) ->
            Just 25

        ( Triple, W (W (W _)) ) ->
            Just 30

        _ ->
            Nothing


basePatchDC : PatchEffectiveness -> CriticalStatus -> Maybe Int
basePatchDC pe cs =
    basePatchDCPatches pe cs.patches


patchSeverity : PatchEffectiveness -> Patches -> Maybe Patches
patchSeverity pe patches =
    case ( pe, patches ) of
        ( Single, G x ) ->
            Just x

        ( Single, M x ) ->
            Just x

        ( Double, M (M x) ) ->
            Just x

        ( Single, W x ) ->
            Just x

        ( Double, W (W x) ) ->
            Just x

        ( Triple, W (W (W x)) ) ->
            Just x

        _ ->
            Nothing


patch : PatchEffectiveness -> CriticalStatus -> CriticalStatus
patch pe criticalStatus =
    patchSeverity pe criticalStatus.patches
        |> Maybe.map (\p -> { criticalStatus | patches = p })
        |> Maybe.withDefault criticalStatus


holdItTogether : Int -> CriticalStatus -> CriticalStatus
holdItTogether currentRound cs =
    { cs | heldTogether = Just currentRound }


quickFix : CriticalStatus -> CriticalStatus
quickFix cs =
    { cs | quickFixed = True }


damageSeverity : CausalSeverity -> Severity
damageSeverity severity =
    case severity of
        Emp _ ->
            Glitching

        CriticalDamage Glitching ->
            Malfunctioning

        CriticalDamage Malfunctioning ->
            Wrecked

        -- TODO: This causes crew damage, where a crew member randomly takes
        -- equal damage to what was dealt to the hull.
        CriticalDamage Wrecked ->
            Wrecked


damage : Maybe Int -> Maybe CriticalStatus -> CriticalStatus
damage mEmpUntilRound mCriticalStatus =
    case ( mEmpUntilRound, mCriticalStatus ) of
        ( Nothing, Just criticalStatus ) ->
            let
                severity =
                    damageSeverity criticalStatus.severity

                patches =
                    case severity of
                        Glitching ->
                            G R

                        Malfunctioning ->
                            M (M (G R))

                        Wrecked ->
                            W (W (W (M (M (G R)))))
            in
            { criticalStatus
                | severity = CriticalDamage severity
                , patches = patches
            }

        ( Just empUntilRound, Just criticalStatus ) ->
            case criticalStatus.severity of
                Emp currentEmpUntilRound ->
                    { criticalStatus
                        | severity = Emp <| max empUntilRound currentEmpUntilRound
                        , patches = G R
                    }

                _ ->
                    criticalStatus

        ( _, Nothing ) ->
            { severity =
                mEmpUntilRound
                    |> Maybe.map Emp
                    |> Maybe.withDefault (CriticalDamage Glitching)
            , patches = G R
            , heldTogether = Nothing
            , quickFixed = False
            }
