module Koivu.Tree
    exposing
        ( Tree
        , NodeInfo
        , allowExpand
        , appendChild
        , createNodeInfo
        , deleteNode
        , distributeQty
        , distributeShare
        , demoTree
        , empty
        , encode
        , getMaxSharable
        , isLockable
        , isUnderfed
        , normalize
        , toggleLock
        , updateShare
        )

{-| A representation of a Classification Tree.


# Basics

@docs Tree, NodeInfo


# Building a tree

@docs empty, demoTree, appendChild, createNodeInfo, deleteNode, toggleLock, updateShare


# Querying a tree

@docs allowExpand, getMaxSharable, isLockable, isUnderfed


# Normalizing a tree

@docs distributeShare, distributeQty, normalize


# Encoding

@docs encode

-}

import Canopy
import Json.Encode as Encode
import Koivu.Settings exposing (Settings)


{-| A Koivu Tree.
-}
type alias Tree =
    Canopy.Node NodeInfo


{-| A node information.
-}
type alias NodeInfo =
    { id : Int
    , label : String
    , qty : Int
    , share : Int
    , locked : Bool
    }


{-| Check whether a node is allowed to append new children.
-}
allowExpand : Settings -> Int -> List Tree -> Bool
allowExpand { maxChildren, maxLevels } level children =
    List.length children < maxChildren && level < maxLevels


{-| Append a NodeInfo child to a Tree.
-}
appendChild : NodeInfo -> NodeInfo -> Tree -> Tree
appendChild target nodeInfo tree =
    tree
        |> Canopy.append target nodeInfo
        |> spreadShareAt target 100


{-| Create a new node to be addable to a given tree.

A unique identifier is generated for this node.

-}
createNodeInfo : Tree -> NodeInfo
createNodeInfo root =
    let
        nextId =
            maxId root + 1
    in
        { id = nextId
        , label = "Node #" ++ toString nextId
        , qty = 0
        , share = 100 // List.length (Canopy.children root) + 1
        , locked = False
        }


{-| A tree JSON encoder.

    demoTree
        |> encode
        |> Json.Encode.encode 2

-}
encode : Tree -> Encode.Value
encode tree =
    Canopy.encode (.label >> Encode.string) tree


{-| Deletes a node, ensuring to re-spread shares to deleted node siblings.
-}
deleteNode : NodeInfo -> Tree -> Tree
deleteNode nodeInfo tree =
    case Canopy.parent nodeInfo tree of
        Just (Canopy.Node parent children) ->
            tree
                |> Canopy.remove nodeInfo
                |> Canopy.replaceChildrenAt parent
                    (children
                        |> List.filter (Canopy.value >> (/=) nodeInfo)
                        |> spreadShare 100
                    )

        Nothing ->
            tree


{-| Distributes a quantity across all nodes in a tree.
-}
distributeQty : Int -> Tree -> Tree
distributeQty qty tree =
    let
        nodeInfo =
            Canopy.value tree

        nodeQty =
            qty * nodeInfo.share // 100
    in
        tree
            |> Canopy.replaceValue { nodeInfo | qty = nodeQty }
            |> Canopy.mapChildren (distributeQty nodeQty)


{-| Distributes shares to a given node siblings in a tree, dealing with a
possibly locked sibling.

If the target node itself is locked, this function is a noop.

-}
distributeShare : NodeInfo -> Int -> Tree -> Tree
distributeShare target share root =
    let
        siblings =
            root |> Canopy.siblings target

        ( totalShare, nbSiblings, excludeLocked ) =
            case siblings |> List.filter (Canopy.value >> .locked) |> List.map Canopy.value of
                [ lockedNodeInfo ] ->
                    ( 100 - lockedNodeInfo.share
                    , List.length siblings - 1
                    , \(Canopy.Node { id } _) -> id /= lockedNodeInfo.id
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
        case root |> Canopy.get target |> Maybe.map (Canopy.value >> .locked) of
            Just True ->
                root

            _ ->
                siblings
                    |> List.filter excludeLocked
                    |> List.foldl (\(Canopy.Node val _) tree -> updateShare val toDistribute tree) root
                    |> updateShare target share


{-| Get maximum share a node can reach.
-}
getMaxSharable : NodeInfo -> Tree -> Int
getMaxSharable target node =
    -- Note: a node share can't be < 1, so we compute the maximum share for this
    -- node with all siblings having a minimum share of 1
    let
        siblings =
            node |> Canopy.siblings target
    in
        case List.filter (Canopy.value >> .locked) siblings of
            [ Canopy.Node lockedNodeInfo _ ] ->
                (100 - lockedNodeInfo.share) - (List.length siblings - 1)

            _ ->
                100 - List.length siblings


{-| Check whether a node can be locked or not. A node can be locked if:

  - it has more than one sibling
  - and none of its sibling are already locked

-}
isLockable : NodeInfo -> Tree -> Bool
isLockable target root =
    let
        siblings =
            Canopy.siblings target root

        noSiblingLocked =
            siblings |> List.all (Canopy.value >> .locked >> not)
    in
        List.length siblings > 1 && noSiblingLocked


{-| Checks whether a tree has all its nodes having the minimum quantity configured
in the `Settings`.
-}
isUnderfed : Int -> Tree -> Bool
isUnderfed min =
    Canopy.any (\nodeInfo -> nodeInfo.qty < min)


maxId : Tree -> Int
maxId tree =
    tree
        |> Canopy.values
        |> List.map .id
        |> List.maximum
        |> Maybe.withDefault 0


{-| Normalize a tree, ensuring all leaves have a minimum quantity assigned.
-}
normalize : Int -> Tree -> Tree
normalize min root =
    if isUnderfed min root then
        let
            nodeInfo =
                Canopy.value root

            increasedQty =
                nodeInfo.qty + 1000
        in
            root
                |> Canopy.replaceValue { nodeInfo | qty = increasedQty }
                |> distributeQty increasedQty
                |> normalize min
    else
        root


{-| Spread shares across a list of nodes.
-}
spreadShare : Int -> List Tree -> List Tree
spreadShare total nodes =
    List.map
        (Canopy.updateValue (\n -> { n | share = total // List.length nodes }))
        nodes


{-| Spread shares to a given node children.
-}
spreadShareAt : NodeInfo -> Int -> Tree -> Tree
spreadShareAt target shares =
    Canopy.updateAt target
        (Canopy.children >> spreadShare shares >> Canopy.node target)


{-| Toggles a locked status of a node. Locking a node means its share value is
locked and can't be modified, so distribution is guaranteed across its siblings
only.
-}
toggleLock : NodeInfo -> Tree -> Tree
toggleLock target =
    Canopy.updateValueAt target (\ni -> { ni | locked = not ni.locked })


{-| Update a node share in a tree.

Note: if share is < 1, it will be forced to 1.

-}
updateShare : NodeInfo -> Int -> Tree -> Tree
updateShare target share =
    Canopy.updateValueAt target
        (\ni ->
            if share > 0 then
                { ni | share = share }
            else
                { ni | share = 1 }
        )



-- Demo fixtures


{-| An empty node.
-}
empty : Tree
empty =
    Canopy.leaf
        { id = 1
        , label = "Source"
        , qty = 0
        , share = 100
        , locked = False
        }


{-| A sample tree, for demo purpose.
-}
demoTree : Tree
demoTree =
    Canopy.node
        { id = 1
        , label = "Source"
        , qty = 0
        , share = 100
        , locked = False
        }
        [ Canopy.node
            { id = 2
            , label = "Avant-vente"
            , qty = 0
            , share = 25
            , locked = False
            }
            [ Canopy.leaf
                { id = 3
                , label = "Lead converti"
                , qty = 0
                , share = 50
                , locked = False
                }
            , Canopy.node
                { id = 10
                , label = "Non converti"
                , qty = 0
                , share = 50
                , locked = False
                }
                [ Canopy.leaf
                    { id = 11
                    , label = "Engagé"
                    , qty = 0
                    , share = 50
                    , locked = False
                    }
                , Canopy.leaf
                    { id = 12
                    , label = "Froid"
                    , qty = 0
                    , share = 50
                    , locked = False
                    }
                ]
            ]
        , Canopy.node
            { id = 4
            , label = "Après vente"
            , qty = 0
            , share = 25
            , locked = False
            }
            [ Canopy.leaf
                { id = 5
                , label = "Pas d'insatisfaction"
                , qty = 0
                , share = 34
                , locked = False
                }
            , Canopy.leaf
                { id = 8
                , label = "Insatisfaction"
                , qty = 0
                , share = 33
                , locked = False
                }
            , Canopy.leaf
                { id = 9
                , label = "Risque d'attrition"
                , qty = 0
                , share = 33
                , locked = False
                }
            ]
        , Canopy.leaf
            { id = 6
            , label = "Autre demande"
            , qty = 0
            , share = 25
            , locked = False
            }
        , Canopy.leaf
            { id = 7
            , label = "Aucune action"
            , qty = 0
            , share = 25
            , locked = False
            }
        ]
