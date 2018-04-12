module Main exposing (main)

import Koivu exposing (Model, Msg(..), init, update, subscriptions, view)
import Koivu.Tree exposing (Settings)
import Html exposing (Html)


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
    Html.program
        { init = Koivu.init settings
        , update = Koivu.update settings
        , subscriptions = Koivu.subscriptions settings
        , view = Koivu.view settings
        }
