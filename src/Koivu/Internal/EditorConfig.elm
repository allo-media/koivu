module Koivu.Internal.EditorConfig exposing (EditorConfig)

import Koivu.Settings exposing (Settings)
import Koivu.Tree exposing (Tree, NodeInfo)


type alias EditorConfig msg =
    { appendNode : NodeInfo -> msg
    , cancelEdit : msg
    , commitLabel : msg
    , deleteNode : NodeInfo -> msg
    , editNode : NodeInfo -> msg
    , editedNode : Maybe NodeInfo
    , root : Tree
    , settings : Settings
    , toggleLock : NodeInfo -> msg
    , updateGlobalQty : Int -> msg
    , updateLabel : NodeInfo -> String -> msg
    , updateShare : NodeInfo -> Int -> msg
    }
