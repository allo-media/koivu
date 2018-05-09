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
        , empty
        , encode
        , findNode
        , findNodes
        , getMaxSharable
        , getProp
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

@docs empty, appendChild, createNodeInfo, deleteNode, toggleLock, updateShare


# Querying a tree

@docs allowExpand, findNode, findNodes, getMaxSharable, getProp, isLockable, isUnderfed


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
allowExpand : Settings -> Int -> Tree -> Bool
allowExpand { maxChildren, maxLevels } level (Canopy.Node _ children) =
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
distributeShare : Int -> Int -> Tree -> Tree
distributeShare share id root =
    case root |> findNode id |> Maybe.map Canopy.value of
        Just target ->
            let
                update toDistribute siblings =
                    siblings
                        |> List.foldl
                            (\sibling -> updateShare sibling (toDistribute // List.length siblings))
                            root
                        |> updateShare target share
            in
                if target.locked then
                    root
                else
                    case lockedSiblings target root of
                        ( Just locked, nonLockedSiblings ) ->
                            nonLockedSiblings |> update (100 - locked.share - share)

                        ( Nothing, nonLockedSiblings ) ->
                            nonLockedSiblings |> update (100 - share)

        Nothing ->
            root


{-| Finds a node by its id in a tree.
-}
findNode : Int -> Tree -> Maybe Tree
findNode id =
    findNodes [ id ] >> List.head


{-| Finds nodes by their id in a tree.
-}
findNodes : List Int -> Tree -> List Tree
findNodes ids =
    Canopy.seek (\ni -> List.member ni.id ids)


{-| Get maximum share a node can reach.
-}
getMaxSharable : Int -> Tree -> Int
getMaxSharable id root =
    -- Note: a node share can't be < 1, so we compute the maximum share for this
    -- node with all siblings having a minimum share of 1
    case root |> findNode id |> Maybe.map Canopy.value of
        Just target ->
            case lockedSiblings target root of
                ( Just locked, others ) ->
                    (100 - locked.share) - List.length others

                ( Nothing, others ) ->
                    100 - List.length others

        Nothing ->
            0


{-| Retrieve a NodeInfo propery from a node.
-}
getProp : (NodeInfo -> a) -> Tree -> a
getProp getter =
    Canopy.value >> getter


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


lockedSiblings : NodeInfo -> Tree -> ( Maybe NodeInfo, List NodeInfo )
lockedSiblings target root =
    let
        siblings =
            root |> Canopy.siblings target |> List.map Canopy.value
    in
        ( siblings |> List.filter .locked |> List.head
        , siblings |> List.filter (\ni -> ni /= target && not ni.locked)
        )


maxId : Tree -> Int
maxId =
    Canopy.flatMap (Canopy.value >> .id)
        >> List.maximum
        >> Maybe.withDefault 0


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
toggleLock : Int -> Tree -> Tree
toggleLock id root =
    case root |> findNode id |> Maybe.map Canopy.value of
        Just target ->
            root |> Canopy.updateValueAt target (\ni -> { ni | locked = not ni.locked })

        Nothing ->
            root


{-| Update a node share in a tree.

Note: if share is < 1, it will be forced to 1.

-}
updateShare : NodeInfo -> Int -> Tree -> Tree
updateShare target share =
    Canopy.updateValueAt target (\ni -> { ni | share = max 1 share })



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
