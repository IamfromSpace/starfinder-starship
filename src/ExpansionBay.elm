module ExpansionBay exposing (..)


type ExpansionBay
    = ArcaneLaboratory
    | CargoHold
    | EscapePods
    | GuestQuarters
    | HangarBay
    | LifeBoats
    | MedicalBay
    | PassengerSeating
    | PowerCoreHousing
    | RecreationSuiteGym
    | RecreationSuiteTrivedDen
    | RecreationSuiteHac
    | ScienceLab
    | SealedEnvironmentChamber
    | ShuttleBay
    | SmugglerCompartment
    | SynthesisBay
    | TechWorkshop


getExpansionBayCost : ExpansionBay -> ( Int, Int )
getExpansionBayCost expansionBay =
    case expansionBay of
        ArcaneLaboratory ->
            ( 1, 1 )

        CargoHold ->
            ( 0, 0 )

        EscapePods ->
            ( 2, 1 )

        GuestQuarters ->
            ( 1, 1 )

        HangarBay ->
            ( 30, 10 )

        LifeBoats ->
            ( 5, 3 )

        MedicalBay ->
            ( 4, 8 )

        PassengerSeating ->
            ( 0, 0 )

        PowerCoreHousing ->
            ( 0, 10 )

        RecreationSuiteGym ->
            ( 0, 1 )

        RecreationSuiteTrivedDen ->
            ( 1, 1 )

        RecreationSuiteHac ->
            ( 3, 1 )

        ScienceLab ->
            ( 2, 1 )

        SealedEnvironmentChamber ->
            ( 2, 1 )

        ShuttleBay ->
            ( 10, 4 )

        SmugglerCompartment ->
            ( 4, 2 )

        SynthesisBay ->
            ( 2, 1 )

        TechWorkshop ->
            ( 3, 1 )


getPowerDraw : ExpansionBay -> Int
getPowerDraw =
    getExpansionBayCost >> Tuple.first


getBuildPoints : ExpansionBay -> Int
getBuildPoints =
    getExpansionBayCost >> Tuple.second
