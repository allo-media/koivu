module Koivu.Internal.NodeHtml exposing (view)

import Canopy
import Koivu.Internal.EditorConfig exposing (EditorConfig)
import Koivu.Tree as Tree exposing (Tree, NodeInfo)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)


maybeIs : Maybe a -> a -> Bool
maybeIs maybeValue candidate =
    case maybeValue of
        Just value ->
            value == candidate

        Nothing ->
            False


labelForm : EditorConfig msg -> NodeInfo -> Html msg
labelForm config nodeInfo =
    Html.form [ onSubmit config.commitLabel ]
        [ input
            [ type_ "text"
            , class "label"
            , id <| "node" ++ toString nodeInfo.id
            , value nodeInfo.label
            , onInput <| config.updateLabel nodeInfo
            , onBlur config.cancelEdit
            ]
            []
        ]


nodeClasses : EditorConfig msg -> Int -> Tree -> Attribute msg
nodeClasses { root, settings } level node =
    let
        nodeInfo =
            Canopy.value node

        children =
            Canopy.children node

        stateClass =
            if List.length children > 0 then
                "trunk"
            else if nodeInfo.qty < settings.minNodeQty then
                "underfed"
            else
                "leaf"
    in
        classList
            [ ( "node", True )
            , ( stateClass, True )
            , ( "is-root", root == node )
            , ( "has-children", List.length children > 0 )
            , ( "can-add-child", Tree.allowExpand settings level children )
            ]


rangeInput : Int -> Int -> Int -> Bool -> (Int -> msg) -> Html msg
rangeInput min max val disabled_ tagger =
    input
        [ type_ "range"
        , class "slider is-fullwidth"
        , Attr.min <| toString min
        , Attr.max <| toString max
        , value <| toString val
        , disabled disabled_
        , onInput
            (\val ->
                val
                    |> String.toInt
                    |> Result.withDefault 0
                    |> tagger
            )
        ]
        []


lockIcon : Bool -> Html msg
lockIcon locked =
    if locked then
        text "🔒"
    else
        text "🔓"


rangeForm : EditorConfig msg -> Tree -> Html msg
rangeForm config node =
    let
        nodeInfo =
            Canopy.value node

        rootInfo =
            Canopy.value config.root

        { globalQty, minNodeQty, maxGlobalQty } =
            config.settings

        isRoot =
            nodeInfo.id == rootInfo.id

        onlyChild =
            (config.root |> Canopy.siblings nodeInfo |> List.length) == 0

        maxSharable =
            Tree.getMaxSharable nodeInfo config.root
    in
        if not isRoot && onlyChild then
            text ""
        else if isRoot then
            rangeInput minNodeQty maxGlobalQty globalQty False config.updateGlobalQty
        else if Tree.isLockable nodeInfo config.root then
            div [ class "with-lock" ]
                [ a [ onClick <| config.toggleLock nodeInfo ] [ lockIcon nodeInfo.locked ]
                , rangeInput 1 maxSharable nodeInfo.share nodeInfo.locked (config.updateShare nodeInfo)
                ]
        else
            rangeInput 1 maxSharable nodeInfo.share False (config.updateShare nodeInfo)


shareInfo : Int -> Int -> Html msg
shareInfo qty share =
    div [ class "share-info" ]
        [ span [ class "quantity" ] [ toString qty |> text ]
        , span [ class "percent" ] [ toString share |> text ]
        ]


deleteBtn : EditorConfig msg -> Tree -> Html msg
deleteBtn { deleteNode, root } node =
    let
        nodeInfo =
            Canopy.value node

        rootInfo =
            Canopy.value root
    in
        if rootInfo.id /= nodeInfo.id then
            button [ class "btn-delete", onClick (deleteNode nodeInfo) ]
                -- FIXME: make these configureable?
                [ text "×" ]
        else
            text ""


appendBtn : EditorConfig msg -> Int -> Tree -> Html msg
appendBtn { appendNode, settings } level node =
    let
        nodeInfo =
            Canopy.value node
    in
        if Tree.allowExpand settings level (Canopy.children node) then
            button [ class "btn-append", onClick (appendNode nodeInfo) ]
                -- FIXME: make these configureable?
                [ text "+" ]
        else
            text ""


view : EditorConfig msg -> Int -> Tree -> Html msg
view ({ editNode, editedNode } as config) level node =
    let
        nodeInfo =
            Canopy.value node
    in
        div
            [ nodeClasses config level node ]
            [ -- editable label
              if maybeIs editedNode nodeInfo then
                labelForm config nodeInfo
              else
                div [ class "actions" ]
                    [ deleteBtn config node
                    , button [ class "btn-edit", onClick (editNode nodeInfo) ]
                        [ nodeInfo.label |> text ]
                    , appendBtn config level node
                    ]

            -- quantity & share
            , shareInfo nodeInfo.qty nodeInfo.share

            -- range slider
            , rangeForm config node
            ]
