module Koivu
    exposing
        ( Model
        , Msg(..)
        , Program
        , setup
        )

{-| An interactive tree representation of [AlloMedia](https://www.allo-media.fr/)'s Customer Path.


# Minimal application setup

    import Koivu
    import Koivu.Tree as Tree
    import Koivu.Settings exposing (Settings)
    import Html

    settings : Settings
    settings =
        { autoNormalize = False
        , globalQty = 100000
        , minNodeQty = 3000
        , maxChildren = 4
        , maxGlobalQty = 200000
        , maxLevels = 3
        , nodeWidth = 140
        , nodeHeight = 80
        , nodePadding = 10
        }

    main : Program Never Koivu.Model Koivu.Msg
    main =
        Tree.demoTree
            |> Koivu.setup settings
            |> Html.program

You'll find a more elaborate integration example [here](https://github.com/allo-media/koivu/tree/2.0.0/example).


# Documentation

@docs setup, Program, Model, Msg

-}

import Canopy
import Dom
import Keyboard
import Koivu.Internal.SvgEditor as SvgEditor
import Koivu.Settings exposing (Settings)
import Koivu.Tree as Tree exposing (Tree, NodeInfo)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task


{-| Koivu main model.
-}
type alias Model =
    { root : Tree
    , editedNode : Maybe NodeInfo
    , settings : Settings
    }


{-| Koivu messages.
-}
type Msg
    = AppendChild NodeInfo
    | CancelEdit
    | CommitLabel
    | DeleteNode NodeInfo
    | EditNode NodeInfo
    | NoOp
    | Normalize
    | SetAutoNormalize Bool
    | ToggleLock NodeInfo
    | UpdateLabel NodeInfo String
    | UpdateQty Int
    | UpdateShare NodeInfo Int


{-| A Koivu program.
-}
type alias Program =
    { init : ( Model, Cmd Msg )
    , subscriptions : Model -> Sub Msg
    , update : Msg -> Model -> ( Model, Cmd Msg )
    , view : Model -> Html Msg
    }


{-| Setup a Koivu program.
-}
setup : Settings -> Tree -> Program
setup settings root =
    { init = init settings root
    , subscriptions = subscriptions
    , update = update
    , view = view
    }


init : Settings -> Tree -> ( Model, Cmd Msg )
init settings root =
    { root = root |> Tree.distributeQty settings.globalQty
    , editedNode = Nothing
    , settings = settings
    }
        ! []


distributeAndNormalize : Settings -> Tree -> Tree
distributeAndNormalize settings root =
    root
        |> Tree.distributeQty settings.globalQty
        |> if settings.autoNormalize then
            Tree.normalize settings.minNodeQty
           else
            identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ settings } as model) =
    case msg of
        AppendChild nodeInfo ->
            let
                childNodeInfo =
                    Tree.createNodeInfo model.root

                newModel =
                    { model
                        | root =
                            model.root
                                |> Tree.appendChild nodeInfo childNodeInfo
                                |> distributeAndNormalize settings
                    }
            in
                newModel |> update (EditNode childNodeInfo)

        CancelEdit ->
            { model | editedNode = Nothing } ! []

        CommitLabel ->
            { model | editedNode = Nothing } ! []

        DeleteNode nodeInfo ->
            { model
                | editedNode = Nothing
                , root =
                    model.root
                        |> Tree.deleteNode nodeInfo
                        |> distributeAndNormalize settings
            }
                ! []

        EditNode nodeInfo ->
            { model | editedNode = Just nodeInfo }
                ! [ ("node" ++ toString nodeInfo.id)
                        |> Dom.focus
                        |> Task.attempt (always NoOp)
                  ]

        NoOp ->
            model ! []

        Normalize ->
            { model | root = model.root |> Tree.normalize settings.minNodeQty } ! []

        SetAutoNormalize autoNormalize ->
            { model
                | root =
                    if autoNormalize then
                        model.root |> Tree.normalize settings.minNodeQty
                    else
                        model.root
                , settings = { settings | autoNormalize = autoNormalize }
            }
                ! []

        ToggleLock nodeInfo ->
            { model
                | editedNode = Nothing
                , root = model.root |> Tree.toggleLock nodeInfo.id
            }
                ! []

        UpdateLabel nodeInfo label ->
            { model
                | root =
                    model.root
                        |> Canopy.updateValueAt nodeInfo (\ni -> { ni | label = label })
            }
                ! []

        UpdateQty qty ->
            { model
                | root = model.root |> distributeAndNormalize settings
                , settings = { settings | globalQty = qty }
            }
                ! []

        UpdateShare nodeInfo share ->
            { model
                | root =
                    model.root
                        |> Tree.distributeShare share nodeInfo.id
                        |> distributeAndNormalize settings
            }
                ! []



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Keyboard.ups
            (\keyCode ->
                if keyCode == 27 then
                    CancelEdit
                else
                    NoOp
            )
        ]



-- Views


formView : Model -> Html Msg
formView ({ settings } as model) =
    div [ class "normalize-form has-text-centered" ]
        [ button
            [ class "button"
            , onClick Normalize
            , disabled <| settings.autoNormalize || not (Tree.isUnderfed settings.minNodeQty model.root)
            ]
            [ text "Normalize" ]
        , label [ class "checkbox" ]
            [ input
                [ type_ "checkbox"
                , onCheck SetAutoNormalize
                , checked settings.autoNormalize
                ]
                []
            , text " Auto"
            ]
        ]


view : Model -> Html Msg
view model =
    let
        editorConfig =
            { appendNode = AppendChild
            , cancelEdit = CancelEdit
            , commitLabel = CommitLabel
            , deleteNode = DeleteNode
            , editNode = EditNode
            , editedNode = model.editedNode
            , root = model.root
            , toggleLock = ToggleLock
            , updateLabel = UpdateLabel
            , updateGlobalQty = UpdateQty
            , updateShare = UpdateShare
            , settings = model.settings
            }
    in
        div [ class "koivu" ]
            [ div [ class "koivu-tree" ]
                [ SvgEditor.view editorConfig model.root ]
            , formView model
            ]
