module Main exposing (..)

import Koivu
import Koivu.Tree as Tree
import Html


settings : Koivu.Settings
settings =
    { autoNormalize = False
    , globalQty = 100000
    , minNodeQty = 3000
    , maxChildren = 4
    , maxGlobalQty = 200000
    , maxLevels = 3
    , nodeWidth = 140
    , nodeHeight = 80
    , nodePadding = 10
    }


main : Program Never Koivu.Model Koivu.Msg
main =
    Tree.demoTree
        |> Koivu.setup settings
        |> Html.program
