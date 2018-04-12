module Koivu
    exposing
        ( Model
        , Msg(..)
        , init
        , update
        , subscriptions
        , view
        )

{-| An interactive tree representation of [AllMedia](https://www.allo-media.fr/)'s Customer Path.


# Configure the tree

@docs Model, Msg, init, subscriptions, update, view

-}

import Koivu.Tree as Tree exposing (Node(..), Settings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard exposing (KeyCode)
import Koivu.SvgEditor as SvgEditor


{-| Main model
-}
type alias Model =
    { root : Node
    , editedNode : Maybe Int
    , qty : Int
    , autoNormalize : Bool
    }


{-| Messages
-}
type Msg
    = AppendChild Int
    | CancelEdit
    | CommitLabel
    | DeleteNode Int
    | EditNode Int
    | KeyUp KeyCode
    | Normalize
    | SetAutoNormalize Bool
    | UpdateLabel Int String
    | UpdateQty Int
    | UpdateShare Int Int


{-| Init
-}
init : Settings -> ( Model, Cmd Msg )
init settings =
    { root = Tree.demoTree |> Tree.distributeQty settings.globalQty
    , editedNode = Nothing
    , qty = settings.globalQty
    , autoNormalize = settings.autoNormalize
    }
        ! []


distributeAndNormalize : Settings -> Int -> Node -> Node
distributeAndNormalize settings qty root =
    root
        |> Tree.distributeQty qty
        |> if settings.autoNormalize then
            Tree.normalize settings.minNodeQty
           else
            identity


{-| Update
-}
update : Settings -> Msg -> Model -> ( Model, Cmd Msg )
update settings msg ({ autoNormalize, qty } as model) =
    case msg of
        AppendChild id ->
            let
                newNode =
                    Tree.createNode model.root

                (Node nodeInfo) =
                    newNode

                newModel =
                    { model
                        | root =
                            model.root
                                |> Tree.appendChild id newNode
                                |> distributeAndNormalize settings qty
                    }
            in
                newModel |> update settings (EditNode nodeInfo.id)

        CancelEdit ->
            { model | editedNode = Nothing } ! []

        CommitLabel ->
            { model | editedNode = Nothing } ! []

        DeleteNode id ->
            { model
                | editedNode = Nothing
                , root =
                    model.root
                        |> Tree.deleteNode id
                        |> distributeAndNormalize settings qty
            }
                ! []

        EditNode id ->
            { model | editedNode = Just id }
                -- FIXME: add a command to the config
                -- ! [ Ports.select <| "node" ++ toString id ]
                ! []

        Normalize ->
            { model | root = model.root |> Tree.normalize settings.minNodeQty } ! []

        KeyUp code ->
            if code == 27 then
                { model | editedNode = Nothing } ! []
            else
                model ! []

        SetAutoNormalize autoNormalize ->
            { model
                | autoNormalize = autoNormalize
                , root =
                    if autoNormalize then
                        model.root |> Tree.normalize settings.minNodeQty
                    else
                        model.root
            }
                ! []

        UpdateLabel id label ->
            { model | root = model.root |> Tree.updateLabel id label } ! []

        UpdateQty qty ->
            { model
                | qty = qty
                , root = model.root |> distributeAndNormalize settings qty
            }
                ! []

        UpdateShare id share ->
            { model
                | root =
                    model.root
                        |> Tree.distributeShare id share
                        |> distributeAndNormalize settings qty
            }
                ! []



-- Subscriptions


{-| Subscriptions
-}
subscriptions : Settings -> Model -> Sub Msg
subscriptions _ _ =
    -- FIXME: this triggers many updates for no reason, couldn't filter Esc
    -- key here ?
    Sub.batch [ Keyboard.ups KeyUp ]



-- Views


formView : Settings -> Model -> Html Msg
formView settings model =
    div [ class "normalize-form has-text-centered" ]
        [ button
            [ class "button"
            , onClick Normalize
            , disabled <| model.autoNormalize || not (Tree.isUnderfed settings.minNodeQty model.root)
            ]
            [ text "Normalize" ]
        , label [ class "checkbox" ]
            [ input
                [ type_ "checkbox"
                , onCheck SetAutoNormalize
                , checked model.autoNormalize
                ]
                []
            , text " Auto"
            ]
        ]


{-| View
-}
view : Settings -> Model -> Html Msg
view settings model =
    let
        editorConfig =
            { appendNode = AppendChild
            , cancelEdit = CancelEdit
            , commitLabel = CommitLabel
            , deleteNode = DeleteNode
            , editNode = EditNode
            , editedNode = model.editedNode
            , root = model.root
            , updateLabel = UpdateLabel
            , updateGlobalQty = UpdateQty
            , updateShare = UpdateShare
            , settings =
                { settings
                    | autoNormalize = model.autoNormalize
                    , globalQty = model.qty
                }
            }
    in
        div [ class "koivu" ]
            [ div [ class "koivu-tree" ] [ SvgEditor.view editorConfig model.root ]
            , formView settings model
            ]
