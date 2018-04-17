module Koivu.Internal.NodeHtml exposing (view)

import Koivu.Internal.EditorConfig exposing (EditorConfig)
import Koivu.Tree as Tree exposing (Node(..))
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


labelForm : EditorConfig msg -> Node -> Html msg
labelForm config (Node nodeInfo) =
    Html.form [ onSubmit config.commitLabel ]
        [ input
            [ type_ "text"
            , class "label"
            , id <| "node" ++ toString nodeInfo.id
            , value nodeInfo.label
            , onInput <| config.updateLabel nodeInfo.id
            , onBlur config.cancelEdit
            ]
            []
        ]


nodeClasses : EditorConfig msg -> Int -> Node -> Attribute msg
nodeClasses { root, settings } level ((Node { children, qty }) as node) =
    let
        stateClass =
            if List.length children > 0 then
                "trunk"
            else if qty < settings.minNodeQty then
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


rangeForm : EditorConfig msg -> Node -> Html msg
rangeForm config node =
    let
        (Node nodeInfo) =
            node

        (Node rootInfo) =
            config.root

        { globalQty, minNodeQty, maxGlobalQty } =
            config.settings

        isRoot =
            nodeInfo.id == rootInfo.id

        onlyChild =
            (config.root |> Tree.getSiblings nodeInfo.id |> List.length) == 0

        maxSharable =
            Tree.getMaxSharable nodeInfo.id config.root
    in
        if not isRoot && onlyChild then
            text ""
        else if isRoot then
            rangeInput minNodeQty maxGlobalQty globalQty False config.updateGlobalQty
        else if Tree.isLockable nodeInfo.id config.root then
            div [ class "with-lock" ]
                [ a [ onClick <| config.toggleLock nodeInfo.id ] [ lockIcon nodeInfo.locked ]
                , rangeInput 1 maxSharable nodeInfo.share nodeInfo.locked (config.updateShare nodeInfo.id)
                ]
        else
            rangeInput 1 maxSharable nodeInfo.share False (config.updateShare nodeInfo.id)


shareInfo : Int -> Int -> Html msg
shareInfo qty share =
    div [ class "share-info" ]
        [ span [ class "quantity" ] [ toString qty |> text ]
        , span [ class "percent" ] [ toString share |> text ]
        ]


deleteBtn : EditorConfig msg -> Node -> Html msg
deleteBtn { deleteNode, root } (Node { id }) =
    let
        (Node rootInfo) =
            root
    in
        if rootInfo.id /= id then
            button [ class "btn-delete", onClick (deleteNode id) ]
                -- FIXME: make these configureable?
                [ text "×" ]
        else
            text ""


appendBtn : EditorConfig msg -> Int -> Node -> Html msg
appendBtn { appendNode, settings } level (Node { id, children }) =
    if Tree.allowExpand settings level children then
        button [ class "btn-append", onClick (appendNode id) ]
            -- FIXME: make these configureable?
            [ text "+" ]
    else
        text ""


view : EditorConfig msg -> Int -> Node -> Html msg
view ({ editNode, editedNode } as config) level node =
    let
        (Node nodeInfo) =
            node
    in
        div
            [ nodeClasses config level node ]
            [ -- editable label
              if maybeIs editedNode nodeInfo.id then
                labelForm config node
              else
                div [ class "actions" ]
                    [ deleteBtn config node
                    , button [ class "btn-edit", onClick (editNode nodeInfo.id) ]
                        [ nodeInfo.label |> text ]
                    , appendBtn config level node
                    ]

            -- quantity & share
            , shareInfo nodeInfo.qty nodeInfo.share

            -- range slider
            , rangeForm config node
            ]
