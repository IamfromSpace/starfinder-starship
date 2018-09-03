module ExpansionBay exposing (ExpansionBay(..), getBuildPoints, getExpansionBayCost, getExpansionBaysUsed, getPowerDraw, getSizeConstraints, isValidSize, toString)

import Size exposing (..)


type ExpansionBay
    = ArcaneLaboratory
    | CargoHold
    | EscapePods
      -- TODO: Guest quarters have the same possible designations
      -- as crew quarters: Common/Good/Luxurious
      -- This is useful if we want to determine the maximum number
      -- of guests that can be transported.
    | GuestQuarters
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


toString : ExpansionBay -> String
toString expansionBay =
    case expansionBay of
        ArcaneLaboratory ->
            "ArcaneLaboratory"

        CargoHold ->
            "CargoHold"

        EscapePods ->
            "EscapePods"

        GuestQuarters ->
            "GuestQuarters"

        HangarBay ->
            "HangarBay"

        LifeBoats ->
            "LifeBoats"

        MedicalBay ->
            "MedicalBay"

        PassengerSeating ->
            "PassengerSeating"

        PowerCoreHousing ->
            "PowerCoreHousing"

        RecreationSuiteGym ->
            "RecreationSuiteGym"

        RecreationSuiteTrivedDen ->
            "RecreationSuiteTrivedDen"

        RecreationSuiteHac ->
            "RecreationSuiteHac"

        ScienceLab ->
            "ScienceLab"

        SealedEnvironmentChamber ->
            "SealedEnvironmentChamber"

        ShuttleBay ->
            "ShuttleBay"

        SmugglerCompartment x ->
            "SmugglerCompartment " ++ String.fromInt x

        SynthesisBay ->
            "SynthesisBay"

        TechWorkshop ->
            "TechWorkshop"


type alias ExpansionBayCost =
    { powerDraw : Int
    , buildPoints : Int
    , expansionBaysUsed : Int
    , sizeConstraints : Maybe (List Size)
    }


getExpansionBayCost : ExpansionBay -> ExpansionBayCost
getExpansionBayCost expansionBay =
    case expansionBay of
        ArcaneLaboratory ->
            ExpansionBayCost 1 1 1 Nothing

        CargoHold ->
            ExpansionBayCost 0 0 1 Nothing

        EscapePods ->
            ExpansionBayCost 2 1 1 Nothing

        GuestQuarters ->
            ExpansionBayCost 1 1 1 Nothing

        HangarBay ->
            ExpansionBayCost 30 10 4 (Just [ Gargantuan, Colossal ])

        LifeBoats ->
            ExpansionBayCost 5 3 1 Nothing

        MedicalBay ->
            ExpansionBayCost 4 8 1 Nothing

        PassengerSeating ->
            ExpansionBayCost 0 0 1 Nothing

        PowerCoreHousing ->
            ExpansionBayCost 0 10 1 (Just [ Medium, Large, Huge, Gargantuan, Colossal ])

        RecreationSuiteGym ->
            ExpansionBayCost 0 1 1 Nothing

        RecreationSuiteTrivedDen ->
            ExpansionBayCost 1 1 1 Nothing

        RecreationSuiteHac ->
            ExpansionBayCost 3 1 1 Nothing

        ScienceLab ->
            ExpansionBayCost 2 1 1 Nothing

        SealedEnvironmentChamber ->
            ExpansionBayCost 2 1 1 Nothing

        ShuttleBay ->
            ExpansionBayCost 10 4 2 (Just [ Huge, Gargantuan, Colossal ])

        SmugglerCompartment dc ->
            ExpansionBayCost ((dc - 1) // 5 + 1) ((dc - 1) // 5 - 1) 1 Nothing

        SynthesisBay ->
            ExpansionBayCost 2 1 1 Nothing

        TechWorkshop ->
            ExpansionBayCost 3 1 1 Nothing


getPowerDraw : ExpansionBay -> Int
getPowerDraw =
    getExpansionBayCost >> .powerDraw


getBuildPoints : ExpansionBay -> Int
getBuildPoints =
    getExpansionBayCost >> .buildPoints


getExpansionBaysUsed : ExpansionBay -> Int
getExpansionBaysUsed =
    getExpansionBayCost >> .expansionBaysUsed


getSizeConstraints : ExpansionBay -> Maybe (List Size)
getSizeConstraints =
    getExpansionBayCost >> .sizeConstraints


isValidSize : Size -> ExpansionBay -> Bool
isValidSize size bay =
    getSizeConstraints bay
        |> Maybe.map (List.member size)
        |> Maybe.withDefault True
