module Main exposing (..)

import Koivu exposing (Model, Msg, setup)
import Koivu.Tree as Tree exposing (Node(..), Settings)
import Html


settings : Settings
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


main : Program Never Model Msg
main =
    Tree.empty
        |> Koivu.setup settings
        |> Html.program
