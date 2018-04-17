module Koivu.Tree
    exposing
        ( Node(..)
        , NodeInfo
        , allowExpand
        , appendChild
        , createNode
        , deleteNode
        , distributeQty
        , distributeShare
        , demoTree
        , empty
        , encode
        , findNode
        , findNodes
        , getMaxSharable
        , getParent
        , getProp
        , getSiblings
        , isLockable
        , isUnderfed
        , normalize
        , toggleLock
        , updateLabel
        , updateShare
        )

{-| A representation of a Classification Tree.


# Basics

@docs Node, NodeInfo


# Building a tree

@docs empty, demoTree, appendChild, createNode, deleteNode, toggleLock, updateLabel, updateShare


# Querying a tree

@docs allowExpand, findNode, findNodes, getMaxSharable, getParent, getProp, getSiblings, isLockable, isUnderfed


# Normalizing a tree

@docs distributeShare, distributeQty, normalize


# Encoding

@docs encode

-}

import Json.Encode as Encode
import Koivu.Settings exposing (Settings)


{-| A tree node.
-}
type Node
    = Node NodeInfo


{-| A tree node information.
-}
type alias NodeInfo =
    { id : Int
    , label : String
    , qty : Int
    , share : Int
    , locked : Bool
    , children : List Node
    }



-- Encoders


{-| A tree JSON encoder.

    demoTree
    |> encode
    |> Json.Encode.encode 2

-}
encode : Node -> Encode.Value
encode (Node nodeInfo) =
    Encode.object
        [ ( "label", Encode.string nodeInfo.label )
        , ( "children", nodeInfo.children |> List.map encode |> Encode.list )
        ]



-- Utils


{-| Check whether a node is allowed to append new children.
-}
allowExpand : Settings -> Int -> List Node -> Bool
allowExpand { maxChildren, maxLevels } level children =
    List.length children < maxChildren && level < maxLevels


{-| Append a new child to a node identified by its id in a tree.
-}
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


{-| Create a new node to be addable to a given tree.

A unique identifier is generated for this node.

-}
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
            , locked = False
            , children = []
            }


{-| Deletes a node from a tree, by its id.
-}
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


{-| Distributes a quantity across all nodes in a tree.
-}
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


{-| Distributes shares to a given node siblings in a tree, dealing with a
possibly locked sibling.

If the target node itself is locked, this function is a noop.

-}
distributeShare : Int -> Int -> Node -> Node
distributeShare id share root =
    let
        siblings =
            root |> getSiblings id

        ( totalShare, nbSiblings, excludeLocked ) =
            case List.filter (getProp .locked) siblings of
                [ Node locked ] ->
                    ( 100 - locked.share
                    , List.length siblings - 1
                    , \(Node { id }) -> id /= locked.id
                    )

                _ ->
                    ( 100, List.length siblings, always True )

        toDistribute =
            case (totalShare - share) // nbSiblings of
                0 ->
                    1

                n ->
                    n
    in
        case root |> findNode id |> Maybe.map (getProp .locked) of
            Just True ->
                root

            _ ->
                siblings
                    |> List.filter excludeLocked
                    |> List.foldl (\(Node { id }) tree -> updateShare id toDistribute tree) root
                    |> updateShare id share


{-| Find a node in a tree, by its id.
-}
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


{-| Find nodes in a tree, by its id.
-}
findNodes : List Int -> Node -> List (Maybe Node)
findNodes ids root =
    ids |> List.map (\id -> findNode id root)


{-| Get maximum share a node can reach.
-}
getMaxSharable : Int -> Node -> Int
getMaxSharable id node =
    -- Note: a node share can't be < 1, so we compute the maximum share for this
    -- node with all siblings having a minimum share of 1
    let
        siblings =
            node |> getSiblings id
    in
        case List.filter (getProp .locked) siblings of
            [ Node locked ] ->
                (100 - locked.share) - (List.length siblings - 1)

            _ ->
                100 - List.length siblings


{-| Retrieve the parent of a given node in a tree, by its id.
-}
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


{-| Helper to retrieve a `NodeInfo` property from a node.
-}
getProp : (NodeInfo -> a) -> Node -> a
getProp getter (Node info) =
    getter info


{-| Retrieve a node siblings identified by its id in a tree.
-}
getSiblings : Int -> Node -> List Node
getSiblings id node =
    case getParent id node of
        Just (Node parentInfo) ->
            parentInfo.children
                |> List.filter (\node -> getProp .id node /= id)

        Nothing ->
            []


{-| Checks whether a tree has all its nodes having the minimum quantity configured
in the `Settings`.
-}
isUnderfed : Int -> Node -> Bool
isUnderfed min (Node root) =
    if root.qty < min then
        True
    else
        root.children
            |> List.filter (isUnderfed min)
            |> (\underfed -> List.length underfed > 0)


{-| Check whether a node can be locked or not. A node can be locked if:

  - it has more than one sibling
  - and none of its sibling are already locked

-}
isLockable : Int -> Node -> Bool
isLockable id root =
    let
        siblings =
            getSiblings id root

        noSiblingLocked =
            List.all (getProp (not << .locked)) siblings
    in
        List.length siblings > 1 && noSiblingLocked


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


{-| Normalize a tree, ensuring all leaves have a minimum quantity assigned.
-}
normalize : Int -> Node -> Node
normalize min ((Node nodeInfo) as node) =
    if isUnderfed min node then
        let
            increasedQty =
                nodeInfo.qty + 1000

            increased =
                Node { nodeInfo | qty = increasedQty }
                    |> distributeQty increasedQty
        in
            normalize min increased
    else
        node


{-| Spread shares across a list of nodes.
-}
spreadShare : Int -> List Node -> List Node
spreadShare total nodes =
    nodes |> List.map (\(Node ni) -> Node { ni | share = total // List.length nodes })


{-| Toggles a locked status of a node. Locking a node means its share value is
locked and can't be modified, so distribution is guaranteed across its siblings
only.
-}
toggleLock : Int -> Node -> Node
toggleLock id (Node root) =
    if root.id == id then
        Node { root | locked = not root.locked }
    else
        Node
            { root
                | children =
                    root.children
                        |> List.map (toggleLock id)
            }


{-| Updates a node in a tree.
-}
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


{-| Update a node label in a tree.
-}
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


{-| Update a node share in a tree.

Note: if share is < 1, it will be forced to 1.

-}
updateShare : Int -> Int -> Node -> Node
updateShare id share (Node root) =
    if root.id == id then
        Node
            { root
                | share =
                    if share > 0 then
                        share
                    else
                        1
            }
    else
        Node
            { root
                | children =
                    root.children
                        |> List.map (updateShare id share)
            }



-- Demo fixtures


{-| An empty node.
-}
empty : Node
empty =
    Node
        { id = 1
        , label = "Source"
        , qty = 0
        , share = 100
        , locked = False
        , children = []
        }


{-| A sample tree, for demo purpose.
-}
demoTree : Node
demoTree =
    Node
        { id = 1
        , label = "Source"
        , qty = 0
        , share = 100
        , locked = False
        , children =
            [ Node
                { id = 2
                , label = "Avant-vente"
                , qty = 0
                , share = 25
                , locked = False
                , children =
                    [ Node
                        { id = 3
                        , label = "Lead converti"
                        , qty = 0
                        , share = 50
                        , locked = False
                        , children = []
                        }
                    , Node
                        { id = 10
                        , label = "Non converti"
                        , qty = 0
                        , share = 50
                        , locked = False
                        , children =
                            [ Node
                                { id = 11
                                , label = "Engagé"
                                , qty = 0
                                , share = 50
                                , locked = False
                                , children = []
                                }
                            , Node
                                { id = 12
                                , label = "Froid"
                                , qty = 0
                                , share = 50
                                , locked = False
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
                , locked = False
                , children =
                    [ Node
                        { id = 5
                        , label = "Pas d'insatisfaction"
                        , qty = 0
                        , share = 34
                        , locked = False
                        , children = []
                        }
                    , Node
                        { id = 8
                        , label = "Insatisfaction"
                        , qty = 0
                        , share = 33
                        , locked = False
                        , children = []
                        }
                    , Node
                        { id = 9
                        , label = "Risque d'attrition"
                        , qty = 0
                        , share = 33
                        , locked = False
                        , children = []
                        }
                    ]
                }
            , Node
                { id = 6
                , label = "Autre demande"
                , qty = 0
                , share = 25
                , locked = False
                , children = []
                }
            , Node
                { id = 7
                , label = "Aucune action"
                , qty = 0
                , share = 25
                , locked = False
                , children = []
                }
            ]
        }
