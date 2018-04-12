module Koivu.SvgEditor exposing (view)

import Koivu.Tree exposing (EditorConfig, Node(..))
import Koivu.NodeHtml as NodeHtml
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import TreeDiagram exposing (Tree, TreeLayout)
import TreeDiagram.Svg as SvgTree


type alias NodeStyle =
    { width : Int
    , height : Int
    }


nodeStyle : NodeStyle
nodeStyle =
    -- FIXME: this should probably be part of settings
    { width = 135, height = 80 }


(=>) : (String -> a) -> b -> a
(=>) prop value =
    prop (toString value)


treeLayout : TreeLayout
treeLayout =
    { orientation = TreeDiagram.topToBottom
    , levelHeight = 100
    , siblingDistance = 200
    , subtreeDistance = 150
    , padding = nodeStyle.height + 20
    }


drawLine : ( Float, Float ) -> Svg msg
drawLine ( targetX, targetY ) =
    Svg.line
        [ x1 => 0
        , y1 => 0
        , x2 => targetX
        , y2 => targetY
        , class "line-path"
        ]
        []


toCenter : String
toCenter =
    let
        padX =
            -(nodeStyle.width // 2)

        padY =
            -(nodeStyle.height // 2)
    in
        "translate(" ++ toString padX ++ " " ++ toString padY ++ ")"


drawNode : EditorConfig msg -> ( Int, Node ) -> Svg msg
drawNode config ( level, node ) =
    Svg.foreignObject
        [ width => nodeStyle.width
        , height => nodeStyle.height
        , transform toCenter
        ]
        [ NodeHtml.view config level node ]


toTree : EditorConfig msg -> Int -> Node -> Tree ( Int, Node )
toTree config level ((Node nodeInfo) as node) =
    List.map (toTree config (level + 1)) nodeInfo.children
        |> TreeDiagram.node ( level, node )


view : EditorConfig msg -> Node -> Svg msg
view config node =
    toTree config 0 node
        |> SvgTree.draw treeLayout (drawNode config) drawLine
