module Koivu.Internal.EditorConfig exposing (EditorConfig)

import Koivu.Settings exposing (Settings)
import Koivu.Tree exposing (Node)


type alias EditorConfig msg =
    { appendNode : Int -> msg
    , cancelEdit : msg
    , commitLabel : msg
    , deleteNode : Int -> msg
    , editNode : Int -> msg
    , editedNode : Maybe Int
    , root : Node
    , settings : Settings
    , updateGlobalQty : Int -> msg
    , updateLabel : Int -> String -> msg
    , updateShare : Int -> Int -> msg
    }
