module Koivu.Tree
    exposing
        ( EditorConfig
        , Node(..)
        , NodeInfo
        , Settings
        , allowExpand
        , appendChild
        , createNode
        , deleteNode
        , distributeShare
        , distributeQty
        , demoTree
        , empty
        , encode
        , findNode
        , findNodes
        , getParent
        , getProp
        , getSiblings
        , isUnderfed
        , normalize
        , updateLabel
        , updateShare
        )

{-| A representation of a Classification Tree.

@docs EditorConfig, Node, NodeInfo, Settings, allowExpand, appendChild, createNode, deleteNode, distributeShare, distributeQty, demoTree, emptyTree, encode, findNode, findNodes, getParent, getProp, getSiblings, isUnderfed, normalize, updateLabel, updateShare

-}

import Json.Encode as Encode


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


type Node
    = Node NodeInfo


type alias NodeInfo =
    { id : Int
    , label : String
    , qty : Int
    , share : Int
    , children : List Node
    }


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



-- Encoders


encode : Node -> Encode.Value
encode (Node nodeInfo) =
    Encode.object
        [ ( "label", Encode.string nodeInfo.label )
        , ( "children", nodeInfo.children |> List.map encode |> Encode.list )
        ]



-- Utils


allowExpand : Settings -> Int -> List Node -> Bool
allowExpand { maxChildren, maxLevels } level children =
    List.length children < maxChildren && level < maxLevels


appendChild : Int -> Node -> Node -> Node
appendChild id newNode (Node root) =
    if root.id == id then
        Node { root | children = spreadShare 100 (newNode :: root.children) }
    else
        Node
            { root
                | children =
                    root.children
                        |> List.map (appendChild id newNode)
            }


createNode : Node -> Node
createNode (Node root) =
    let
        id =
            maxId root.children + 1
    in
        Node
            { id = id
            , label = "Node #" ++ toString id
            , qty = 0
            , share = 100 // List.length root.children + 1
            , children = []
            }


deleteNode : Int -> Node -> Node
deleteNode id root =
    case getParent id root of
        Just (Node parent) ->
            let
                newChildren =
                    parent.children
                        |> List.filter (\node -> getProp .id node /= id)
                        |> spreadShare 100
            in
                root |> updateNode parent.id (Node { parent | children = newChildren })

        Nothing ->
            root


distributeQty : Int -> Node -> Node
distributeQty qty (Node node) =
    let
        nodeQty =
            qty * node.share // 100
    in
        Node
            { node
                | qty = nodeQty
                , children = node.children |> List.map (distributeQty nodeQty)
            }


distributeShare : Int -> Int -> Node -> Node
distributeShare id share node =
    let
        siblings =
            node |> getSiblings id

        toDistribute =
            case (100 - share) // List.length siblings of
                0 ->
                    1

                n ->
                    n
    in
        siblings
            |> List.foldl (\(Node { id }) tree -> updateShare id toDistribute tree) node
            |> updateShare id share


findNode : Int -> Node -> Maybe Node
findNode id root =
    if getProp .id root == id then
        Just root
    else
        root
            |> getProp .children
            |> List.map (findNode id)
            |> List.filter ((/=) Nothing)
            |> List.head
            |> Maybe.withDefault Nothing


findNodes : List Int -> Node -> List (Maybe Node)
findNodes ids root =
    ids |> List.map (\id -> findNode id root)


getParent : Int -> Node -> Maybe Node
getParent id root =
    root
        |> getProp .children
        |> List.foldl
            (\node acc ->
                case acc of
                    Just found ->
                        Just found

                    Nothing ->
                        if getProp .id node == id then
                            Just root
                        else
                            getParent id node
            )
            Nothing


getProp : (NodeInfo -> a) -> Node -> a
getProp getter (Node info) =
    getter info


getSiblings : Int -> Node -> List Node
getSiblings id node =
    case getParent id node of
        Just (Node parentInfo) ->
            parentInfo.children
                |> List.filter (\node -> getProp .id node /= id)

        Nothing ->
            []


isUnderfed : Int -> Node -> Bool
isUnderfed min (Node root) =
    if root.qty < min then
        True
    else
        root.children
            |> List.filter (isUnderfed min)
            |> (\underfed -> List.length underfed > 0)


maxId : List Node -> Int
maxId nodes =
    nodes
        |> List.map
            (\(Node { id, children }) ->
                let
                    max =
                        maxId children
                in
                    if id > max then
                        id
                    else
                        max
            )
        |> List.maximum
        |> Maybe.withDefault 1


normalize : Int -> Node -> Node
normalize min ((Node nodeInfo) as node) =
    if isUnderfed min node then
        let
            newQty =
                nodeInfo.qty + 1000

            increased =
                Node { nodeInfo | qty = newQty }
                    |> distributeQty newQty
        in
            normalize min increased
    else
        node


updateNode : Int -> Node -> Node -> Node
updateNode id node (Node root) =
    if root.id == id then
        node
    else
        Node
            { root
                | children =
                    root.children
                        |> List.map (updateNode id node)
            }


spreadShare : Int -> List Node -> List Node
spreadShare total nodes =
    nodes |> List.map (\(Node ni) -> Node { ni | share = total // List.length nodes })


updateLabel : Int -> String -> Node -> Node
updateLabel id label (Node root) =
    if root.id == id then
        Node { root | label = label }
    else
        Node
            { root
                | children =
                    root.children
                        |> List.map (updateLabel id label)
            }


updateShare : Int -> Int -> Node -> Node
updateShare id share (Node root) =
    if root.id == id then
        Node { root | share = share }
    else
        Node
            { root
                | children =
                    root.children
                        |> List.map (updateShare id share)
            }



-- Demo fixtures


empty : Node
empty =
    Node
        { id = 1
        , label = "Source"
        , qty = 0
        , share = 100
        , children = []
        }


demoTree : Node
demoTree =
    Node
        { id = 1
        , label = "Source"
        , qty = 0
        , share = 100
        , children =
            [ Node
                { id = 2
                , label = "Avant-vente"
                , qty = 0
                , share = 25
                , children =
                    [ Node
                        { id = 3
                        , label = "Lead converti"
                        , qty = 0
                        , share = 50
                        , children = []
                        }
                    , Node
                        { id = 10
                        , label = "Non converti"
                        , qty = 0
                        , share = 50
                        , children =
                            [ Node
                                { id = 11
                                , label = "Engagé"
                                , qty = 0
                                , share = 50
                                , children = []
                                }
                            , Node
                                { id = 12
                                , label = "Froid"
                                , qty = 0
                                , share = 50
                                , children = []
                                }
                            ]
                        }
                    ]
                }
            , Node
                { id = 4
                , label = "Après vente"
                , qty = 0
                , share = 25
                , children =
                    [ Node
                        { id = 5
                        , label = "Pas d'insatisfaction"
                        , qty = 0
                        , share = 34
                        , children = []
                        }
                    , Node
                        { id = 8
                        , label = "Insatisfaction"
                        , qty = 0
                        , share = 33
                        , children = []
                        }
                    , Node
                        { id = 9
                        , label = "Risque d'attrition"
                        , qty = 0
                        , share = 33
                        , children = []
                        }
                    ]
                }
            , Node
                { id = 6
                , label = "Autre demande"
                , qty = 0
                , share = 25
                , children = []
                }
            , Node
                { id = 7
                , label = "Aucune action"
                , qty = 0
                , share = 25
                , children = []
                }
            ]
        }
