module Koivu
    exposing
        ( Model
        , Msg
        , Program
        , setup
        )

{-| An interactive tree representation of [AllMedia](https://www.allo-media.fr/)'s Customer Path.


# Minimal application setup

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

    main : Program Never Model Msg
    main =
        Tree.demoTree
            |> Koivu.setup settings
            |> Html.program


# Documentation

@docs Program, Model, Msg, setup

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
    , settings : Settings
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
setup : Settings -> Node -> Program
setup settings root =
    { init = init settings root
    , subscriptions = subscriptions
    , update = update
    , view = view
    }


init : Settings -> Node -> ( Model, Cmd Msg )
init settings root =
    { root = root |> Tree.distributeQty settings.globalQty
    , editedNode = Nothing
    , settings = settings
    }
        ! []


distributeAndNormalize : Settings -> Node -> Node
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
                                |> distributeAndNormalize settings
                    }
            in
                newModel |> update (EditNode nodeInfo.id)

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
                        |> distributeAndNormalize settings
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
                | root =
                    if autoNormalize then
                        model.root |> Tree.normalize settings.minNodeQty
                    else
                        model.root
                , settings = { settings | autoNormalize = autoNormalize }
            }
                ! []

        UpdateLabel id label ->
            { model | root = model.root |> Tree.updateLabel id label } ! []

        UpdateQty qty ->
            { model
                | root = model.root |> distributeAndNormalize settings
                , settings = { settings | globalQty = qty }
            }
                ! []

        UpdateShare id share ->
            { model
                | root =
                    model.root
                        |> Tree.distributeShare id share
                        |> distributeAndNormalize settings
            }
                ! []



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    -- FIXME: this triggers many updates for no reason, couldn't filter Esc
    -- key here ?
    Sub.batch [ Keyboard.ups KeyUp ]



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
