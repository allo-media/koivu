module Koivu.Internal.SvgEditor exposing (view)

import Canopy
import Koivu.Internal.EditorConfig exposing (EditorConfig)
import Koivu.Internal.NodeHtml as NodeHtml
import Koivu.Settings exposing (Settings)
import Koivu.Tree as Tree exposing (Tree)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import TreeDiagram
import TreeDiagram.Svg as SvgTree


(=>) : (String -> a) -> b -> a
(=>) prop value =
    prop (toString value)


treeLayout : Settings -> TreeDiagram.TreeLayout
treeLayout { nodeWidth, nodeHeight, nodePadding } =
    { orientation = TreeDiagram.topToBottom
    , levelHeight = nodeHeight + nodePadding
    , siblingDistance = nodeWidth + nodePadding
    , subtreeDistance = nodeWidth + nodePadding
    , padding = nodeHeight + nodePadding
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


toCenter : Settings -> String
toCenter { nodeWidth, nodeHeight } =
    let
        padX =
            -(nodeWidth // 2)

        padY =
            -(nodeHeight // 2)
    in
        "translate(" ++ toString padX ++ " " ++ toString padY ++ ")"


drawNode : EditorConfig msg -> ( Int, Tree.Tree ) -> Svg msg
drawNode ({ settings } as config) ( level, node ) =
    Svg.foreignObject
        [ width => settings.nodeWidth
        , height => settings.nodeHeight
        , transform (toCenter settings)
        ]
        [ NodeHtml.view config level node ]


toTree : EditorConfig msg -> Int -> Tree -> TreeDiagram.Tree ( Int, Tree )
toTree config level node =
    List.map (toTree config (level + 1)) (Canopy.children node)
        |> TreeDiagram.node ( level, node )


view : EditorConfig msg -> Tree -> Svg msg
view config node =
    toTree config 0 node
        |> SvgTree.draw (treeLayout config.settings) (drawNode config) drawLine
