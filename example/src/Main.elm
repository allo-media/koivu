module Main exposing (..)

import Canopy
import Koivu exposing (Msg(..))
import Koivu.Tree as Tree exposing (Tree)
import Koivu.Settings exposing (Settings)
import Html exposing (Html)
import Ports


type alias Model =
    Koivu.Model


type Msg
    = KoivuMsg Koivu.Msg


{-| A sample tree, for demo purpose.
-}
demoTree : Tree
demoTree =
    Canopy.node
        { id = 1, label = "Source", qty = 0, share = 100, locked = False }
        [ Canopy.node
            { id = 2, label = "Avant-vente", qty = 0, share = 25, locked = False }
            [ Canopy.leaf { id = 3, label = "Lead converti", qty = 0, share = 50, locked = False }
            , Canopy.node { id = 10, label = "Non converti", qty = 0, share = 50, locked = False }
                [ Canopy.leaf { id = 11, label = "Engagé", qty = 0, share = 50, locked = False }
                , Canopy.leaf { id = 12, label = "Froid", qty = 0, share = 50, locked = False }
                ]
            ]
        , Canopy.node { id = 4, label = "Après vente", qty = 0, share = 25, locked = False }
            [ Canopy.leaf { id = 5, label = "Pas d'insatisfaction", qty = 0, share = 34, locked = False }
            , Canopy.leaf { id = 8, label = "Insatisfaction", qty = 0, share = 33, locked = False }
            , Canopy.leaf { id = 9, label = "Risque d'attrition", qty = 0, share = 33, locked = False }
            ]
        , Canopy.leaf { id = 6, label = "Autre demande", qty = 0, share = 25, locked = False }
        , Canopy.leaf { id = 7, label = "Aucune action", qty = 0, share = 25, locked = False }
        ]


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


koivu : Koivu.Program
koivu =
    demoTree
        |> Koivu.setup settings


mapKoivuCmd : ( Model, Cmd Koivu.Msg ) -> ( Model, Cmd Msg )
mapKoivuCmd ( model, msgKoivuCmd ) =
    ( model, msgKoivuCmd |> Cmd.map KoivuMsg )


init : ( Model, Cmd Msg )
init =
    koivu.init
        |> mapKoivuCmd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KoivuMsg koivuMsg ->
            let
                ( koivuModel, koivuCmds ) =
                    koivu.update koivuMsg model
            in
                ( koivuModel
                , Cmd.batch
                    [ Cmd.map KoivuMsg koivuCmds
                    , case koivuMsg of
                        EditNode nodeInfo ->
                            Ports.select <| "node" ++ toString nodeInfo.id

                        _ ->
                            Cmd.none
                    ]
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    koivu.subscriptions model
        |> Sub.map KoivuMsg


view : Model -> Html Msg
view model =
    koivu.view model
        |> Html.map KoivuMsg


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
