module Status exposing (CriticalStatus, PatchableSystem(..), Severity(..), Status, areShieldsFull, balanceEvenly, balanceFromArc, balanceToAll, canBalanceFromTo, damage, damageArc, damageSeverity, damageSystem, divertPowerToShields, getEffectiveCriticalStatus, holdItTogether, maxDivertPowerToShieldPoints, patchCriticalStatus, patchStatus, pickPatchableSystem, quickFix, tick, tickCriticalStatus, updateCriticalStatus)

import Arc exposing (AnArc, Arc)
import Random exposing (Generator)
import Starship exposing (Starship)
import Switch exposing (Switch(..))
import Togglable exposing (extract, meta)


type Severity
    = Glitching
    | Malfunctioning
    | Wrecked


type alias CriticalStatus =
    { severity : Severity
    , remainingRounds :
        -- If Nothing, the effect last until repaired
        -- TODO: This is only via EMP, only applies Glitching, and has no effect on systems that are critically damaged normally
        Maybe Int
    , patches : Int
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


applyPatches : Int -> Severity -> Maybe Severity
applyPatches count severity =
    case severity of
        Wrecked ->
            if count < 3 then
                Just Wrecked

            else if count < 5 then
                Just Malfunctioning

            else if count < 6 then
                Just Glitching

            else
                Nothing

        Malfunctioning ->
            if count < 2 then
                Just Malfunctioning

            else if count < 3 then
                Just Glitching

            else
                Nothing

        Glitching ->
            if count < 1 then
                Just Glitching

            else
                Nothing


applyHoldTogether : Bool -> Severity -> Maybe Severity
applyHoldTogether heldTogether severity =
    case ( heldTogether, severity ) of
        ( True, Wrecked ) ->
            Just Glitching

        ( True, _ ) ->
            Nothing

        ( False, _ ) ->
            Just severity


getEffectiveCriticalStatus : CriticalStatus -> Maybe Severity
getEffectiveCriticalStatus cs =
    if cs.quickFixed then
        Nothing

    else
        applyPatches cs.patches cs.severity
            |> Maybe.andThen (applyHoldTogether cs.heldTogether)



-- TODO: Engineers can take a harder check to reduce difficulty, but this isn't
-- as simple as just earning multiple patches (beacuse you can't just
-- go straight from Malfunctioning -> Nothing by applying 3 patches,
-- you must pass through each severity)


patchCriticalStatus : CriticalStatus -> Maybe CriticalStatus
patchCriticalStatus criticalStatus =
    Maybe.map
        (always { criticalStatus | patches = criticalStatus.patches + 1 })
        (getEffectiveCriticalStatus criticalStatus)


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
                    { criticalStatus
                        | severity = damageSeverity criticalStatus.severity
                        , patches = 0
                    }

        Nothing ->
            { severity = Glitching
            , remainingRounds = rounds
            , patches = 0
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


holdItTogether : PatchableSystem -> Status -> Status
holdItTogether =
    updateCriticalStatus (Maybe.map (\s -> { s | heldTogether = True }))


quickFix : PatchableSystem -> Status -> Status
quickFix =
    updateCriticalStatus (Maybe.map (\s -> { s | quickFixed = True }))


damageSystem : Maybe Int -> PatchableSystem -> Status -> Status
damageSystem rounds =
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


damageArc : Bool -> Starship -> AnArc -> Int -> Status -> ( Int, Status )
damageArc wasCrit build arc amount status =
    let
        criticalThreshold =
            Starship.getMaxHitPoints build // 5

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
        ( 0, { status | shields = Arc.updateArc (\x -> x - amount) arc status.shields } )

    else
        let
            oldCriticalThresholdCount =
                status.damage // criticalThreshold

            newCriticalThresholdCount =
                (status.damage + hullDamage) // criticalThreshold

            critCount =
                (newCriticalThresholdCount
                    - oldCriticalThresholdCount
                )
                    + (if wasCrit then
                        1

                       else
                        0
                      )
        in
        ( critCount
        , { status
            | shields = Arc.updateArc (always 0) arc status.shields
            , damage = status.damage + hullDamage
          }
        )


maxMovableShieldPoints : Starship -> AnArc -> Arc.Arc Int -> Int
maxMovableShieldPoints starship arc shields =
    let
        -- TODO: should this round up or down?
        minRemaining =
            round (toFloat (extract starship.shields).shieldPoints * 0.1)
    in
    Arc.getArc arc shields - minRemaining


balanceEvenly : Starship -> Status -> Status
balanceEvenly starship ({ shields } as status) =
    let
        total =
            Arc.sum shields

        quarter =
            total // 4

        leftover =
            total - 4 * quarter

        newShields =
            shields
                -- Add 1/4 to each
                |> Arc.map (always quarter)
                -- add the leftover to the forward arc
                |> Arc.setArc ((+) leftover) Arc.Forward
    in
    { status | shields = newShields }


balanceToAll_ : Starship -> AnArc -> Int -> Arc.Arc Int -> Maybe (Arc.Arc Int)
balanceToAll_ starship arc amount shields =
    let
        maxMovable =
            maxMovableShieldPoints starship arc shields

        leftover =
            amount - 3 * (amount // 3)
    in
    if amount > maxMovable then
        Nothing

    else
        shields
            -- Add 1/3rd of the amount to _each_ arc (cleaned up in the next step)
            |> Arc.map (\x -> x + amount // 3)
            -- remove the extra 1/3rd and the target amount from the 'from' arc
            |> Arc.setArc (\x -> x - amount - amount // 3) arc
            -- add the leftover to the forward arc
            |> Arc.setArc (\x -> x + leftover) Arc.Forward
            |> Just


balanceToAll : Starship -> AnArc -> Int -> Status -> Maybe Status
balanceToAll starship arc amount status =
    Maybe.map
        (\shields -> { status | shields = shields })
        (balanceToAll_ starship arc amount status.shields)


moveShieldPoints_ : Starship -> AnArc -> AnArc -> Int -> Arc.Arc Int -> Maybe (Arc.Arc Int)
moveShieldPoints_ starship from to amount shields =
    let
        maxMovable =
            maxMovableShieldPoints starship from shields
    in
    if amount > maxMovable then
        Nothing

    else
        shields
            -- Add 1/3rd of the amount to _each_ arc (cleaned up in the next step)
            |> Arc.setArc (\x -> x - amount) from
            -- add the leftover to the forward arc
            |> Arc.setArc (\x -> x + amount) to
            |> Just


moveShieldPoints : Starship -> AnArc -> AnArc -> Int -> Status -> Maybe Status
moveShieldPoints starship from to amount status =
    Maybe.map
        (\shields -> { status | shields = shields })
        (moveShieldPoints_ starship from to amount status.shields)


maxDivertPowerToShieldPoints_ : Starship -> Arc Int -> Int
maxDivertPowerToShieldPoints_ starship shields =
    let
        maxProvidedByPowerCore =
            starship.powerCoreUnits // 20

        maxTotalShieldPoints =
            (extract starship.shields).shieldPoints

        currentTotalShieldPoints =
            Arc.sum shields

        maxAddable =
            maxTotalShieldPoints - currentTotalShieldPoints
    in
    min maxProvidedByPowerCore maxAddable


maxDivertPowerToShieldPoints : Starship -> Status -> Int
maxDivertPowerToShieldPoints starship status =
    maxDivertPowerToShieldPoints_ starship status.shields


divertPowerToShields_ : Starship -> Arc Int -> Arc Int -> Maybe (Arc Int)
divertPowerToShields_ starship added shields =
    let
        allPositive =
            added
                |> Arc.map (\x -> x >= 0)
                |> Arc.all

        pointsAdded =
            Arc.sum added

        maxAddable =
            maxDivertPowerToShieldPoints_ starship shields
    in
    if not allPositive || pointsAdded /= maxAddable then
        Nothing

    else
        Just <| Arc.liftA2 (+) added shields


divertPowerToShields : Starship -> Arc Int -> Status -> Maybe Status
divertPowerToShields starship added status =
    Maybe.map
        (\shields -> { status | shields = shields })
        (divertPowerToShields_ starship added status.shields)


areShieldsFull : Starship -> Status -> Bool
areShieldsFull starship status =
    Arc.sum status.shields >= (extract starship.shields).shieldPoints



-- Given an arc, we check which arcs it can send shield points to via the
-- balance action.  At the end of balancing, all arcs must contain at least 10%
-- of the current total, which means there are three possiblities: we can
-- balance to any arc, we can balance to exactly one arc in need, or we cannot
-- balance to any.  In the latter case it may be because the from arc is less
-- than 10%, because it doesn't have enough to give to put a to arc at or above
-- 10%, or if there are two or more arcs below 10% we can't satisfy the 10%
-- minimum rule.  This rule is dumb.


canBalanceFromTo : AnArc -> Arc Int -> List AnArc
canBalanceFromTo from shields =
    let
        tenPercent =
            Arc.sum shields // 10

        -- We determine how many points we can share from the from arc, and
        -- then which arcs are in need of points to get to 10% (and the number
        -- of points required to do so)
        ( sharablePoints, quadrantsInNeed ) =
            Arc.foldWithAnArc
                (\arc excess ( sharablePoints_, quadrantsInNeed_ ) ->
                    if arc == from then
                        ( excess, quadrantsInNeed_ )

                    else
                        ( sharablePoints_
                        , (if excess < 0 then
                            (::) ( arc, excess )

                           else
                            identity
                          )
                            quadrantsInNeed_
                        )
                )
                ( 0, [] )
                (Arc.map (\x -> x - tenPercent) shields)
    in
    case ( quadrantsInNeed, sharablePoints > 0 ) of
        -- If the from quadrant exceeds 10% and all other quadrants are not
        -- below, then we can balance to any arc
        ( [], True ) ->
            List.filter ((/=) from)
                [ Arc.Forward
                , Arc.Aft
                , Arc.Port
                , Arc.Starboard
                ]

        -- If the from quadrant exceeds 10% and there is exactly one quadrant
        -- that is below 10% _and_ the from quadrant has sufficient points to
        -- push it beyond 10%, then we can balance to that quadrant (and _only_
        -- that quadrant)
        ( [ ( needArc, needExcess ) ], True ) ->
            if sharablePoints >= needExcess then
                [ needArc ]

            else
                []

        -- In any other case, we cannot balance from this arc at all
        _ ->
            []


balanceFromArc : Starship -> ( AnArc, AnArc, Int ) -> Status -> Maybe Status
balanceFromArc starship ( from, to, amount ) status =
    let
        tenPercent =
            Arc.sum status.shields // 10

        asArc =
            Arc.pureWithAnArc
                (\arc ->
                    if arc == from then
                        -amount

                    else if arc == to then
                        amount

                    else
                        0
                )

        afterBalance =
            { status | shields = Arc.liftA2 (+) status.shields asArc }

        validFromTo =
            List.member to (canBalanceFromTo from status.shields)

        allAboveTenPercent =
            afterBalance.shields
                |> Arc.map ((<=) tenPercent)
                |> Arc.all
    in
    if validFromTo && allAboveTenPercent then
        Just afterBalance

    else
        Nothing
