module ExpansionBay exposing (..)

import Size exposing (..)


type ExpansionBay
    = ArcaneLaboratory
    | CargoHold
    | EscapePods
    | GuestQuarters
      -- TODO: A HangarBay has
      --  1) Size requirement
      --  2) Consumes _multiple_ expansion bay slots
      -- Neither of these things are currently modeled.
    | HangarBay
    | LifeBoats
    | MedicalBay
    | PassengerSeating
    | PowerCoreHousing
    | RecreationSuiteGym
    | RecreationSuiteTrivedDen
    | RecreationSuiteHac
      -- TODO: there are two different types of science lab
    | ScienceLab
    | SealedEnvironmentChamber
    | ShuttleBay
    | SmugglerCompartment Int
    | SynthesisBay
    | TechWorkshop


getExpansionBayCost : ExpansionBay -> ( Int, Int, Int, Maybe (List Size) )
getExpansionBayCost expansionBay =
    case expansionBay of
        ArcaneLaboratory ->
            ( 1, 1, 1, Nothing )

        CargoHold ->
            ( 0, 0, 1, Nothing )

        EscapePods ->
            ( 2, 1, 1, Nothing )

        GuestQuarters ->
            ( 1, 1, 1, Nothing )

        HangarBay ->
            ( 30, 10, 4, Nothing )

        LifeBoats ->
            ( 5, 3, 1, Nothing )

        MedicalBay ->
            ( 4, 8, 1, Nothing )

        PassengerSeating ->
            ( 0, 0, 1, Nothing )

        PowerCoreHousing ->
            ( 0, 10, 1, Just [ Medium, Large, Huge, Gargantuan, Colossal ] )

        RecreationSuiteGym ->
            ( 0, 1, 1, Nothing )

        RecreationSuiteTrivedDen ->
            ( 1, 1, 1, Nothing )

        RecreationSuiteHac ->
            ( 3, 1, 1, Nothing )

        ScienceLab ->
            ( 2, 1, 1, Nothing )

        SealedEnvironmentChamber ->
            ( 2, 1, 1, Nothing )

        ShuttleBay ->
            ( 10, 4, 2, Just [ Huge, Gargantuan, Colossal ] )

        SmugglerCompartment dc ->
            ( (dc - 1) // 5 + 1, (dc - 1) // 5 - 1, 1, Nothing )

        SynthesisBay ->
            ( 2, 1, 1, Nothing )

        TechWorkshop ->
            ( 3, 1, 1, Nothing )


getPowerDraw : ExpansionBay -> Int
getPowerDraw =
    getExpansionBayCost >> (\( x, _, _, _ ) -> x)


getBuildPoints : ExpansionBay -> Int
getBuildPoints =
    getExpansionBayCost >> (\( _, x, _, _ ) -> x)


getExpansionBaysUsed : ExpansionBay -> Int
getExpansionBaysUsed =
    getExpansionBayCost >> (\( _, _, x, _ ) -> x)


getSizeConstraints : ExpansionBay -> Maybe (List Size)
getSizeConstraints =
    getExpansionBayCost >> (\( _, _, _, x ) -> x)


isValidSize : Size -> ExpansionBay -> Bool
isValidSize size bay =
    getSizeConstraints bay
        |> Maybe.map (List.member size)
        |> Maybe.withDefault True
