module Computer exposing (..)


type alias Computer =
    { bonus : Int
    , nodes : Int
    }


getPowerDraw : Computer -> Int
getPowerDraw { bonus } =
    bonus * 5 + 5


getBuildPoints : Computer -> Int
getBuildPoints { bonus, nodes } =
    bonus * bonus * nodes
