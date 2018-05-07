module Main exposing (..)

import Koivu exposing (Msg(..))
import Koivu.Tree as Tree
import Koivu.Settings exposing (Settings)
import Html exposing (Html)
import Ports


type alias Model =
    Koivu.Model


type Msg
    = KoivuMsg Koivu.Msg


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
    Tree.demoTree
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
