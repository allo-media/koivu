module Koivu.Settings exposing (Settings)

{-| Koivu settings.

@docs Settings

-}


{-| Settings description:

  - `autoNormalize`: enable auto-normalization of the tree
  - `globalQty`: initial global available quantity
  - `minNodeQty`: minimal quantity for a viable tree
  - `maxChildren`: maximum number of children per node
  - `maxGlobalQty`: maximum global available quantity
  - `maxLevels`: maximum depth for the tree
  - `nodeWidth`: node width, in pixels
  - `nodeHeight`: node height, in pixels
  - `nodePadding`: node padding, in pixels

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
