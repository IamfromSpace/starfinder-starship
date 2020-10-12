module Status exposing (CriticalStatus, PatchableSystem(..), Severity(..), Status, areShieldsFull, balanceEvenly, balanceToAll, damage, damageArc, damageSeverity, damageSystem, divertPowerToShields, getEffectiveCriticalStatus, holdItTogether, maxDivertPowerToShieldPoints, patchCriticalStatus, patchSeverity, patchStatus, pickPatchableSystem, quickFix, tick, tickCriticalStatus, updateCriticalStatus)

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



-- TODO: should this just enforce that all points are consumed (or shields are maxed out)?  Why would you _not_ take points?


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
    if not allPositive || pointsAdded > maxAddable then
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
