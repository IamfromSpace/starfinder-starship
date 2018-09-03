module Computer exposing (Computer, getBuildPoints, getPowerDraw)


type alias Computer =
    { bonus : Int
    , nodes : Int
    }


getPowerDraw : Computer -> Int
getPowerDraw { bonus, nodes } =
    if bonus > 0 && nodes > 0 then
        bonus * 5 + 5

    else
        0


getBuildPoints : Computer -> Int
getBuildPoints { bonus, nodes } =
    bonus * bonus * nodes
