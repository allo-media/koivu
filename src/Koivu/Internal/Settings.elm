module Koivu.Internal.Settings exposing (Settings)


type alias Settings =
    { autoNormalize : Bool
    , globalQty : Int
    , maxChildren : Int
    , maxGlobalQty : Int
    , maxLevels : Int
    , minNodeQty : Int
    , nodeWidth : Int
    , nodeHeight : Int
    , nodePadding : Int
    }
