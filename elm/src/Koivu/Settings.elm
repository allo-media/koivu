module Koivu.Settings exposing (Settings)

{-| sdkj

@docs Settings

-}


{-| Koivu settings.

  - `autoNormalize`: enable auto-normalization of the tree
  - `globalQty`: initial global available quantity
  - `minNodeQty`: minimal quantity for a viable tree
  - `maxChildren`: maximum number of children per node
  - `maxGlobalQty`: maximum global available quantity
  - `maxLevels`: maximum depth for the tree
  - `nodeWidth`: node width
  - `nodeHeight`: node height
  - `nodePadding`: node padding

-}
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
