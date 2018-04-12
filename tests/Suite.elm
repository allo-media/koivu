module Suite exposing (suite)

import Koivu.Tree as Tree exposing (Node(..), NodeInfo)
import Expect exposing (Expectation)
import Test exposing (..)


asTest : String -> Expectation -> Test
asTest label expectation =
    expectation |> always |> test label


suite : Test
suite =
    describe "Data.CustomerPath"
        [ describe "findNode"
            [ Tree.demoTree
                |> Tree.findNode 1
                |> Expect.equal (Just Tree.demoTree)
                |> asTest "should find root node"
            , Tree.demoTree
                |> Tree.findNode 12
                |> Maybe.map (Tree.getProp .label)
                |> Expect.equal (Just "Froid")
                |> asTest "should find a deeply nested node"
            , Tree.demoTree
                |> Tree.findNode 999
                |> Expect.equal (Nothing)
                |> asTest "should not find a non-existent node"
            ]
        , describe "distributeShare"
            [ Tree.demoTree
                |> Tree.distributeShare 11 40
                |> Tree.findNode 12
                |> Maybe.map (Tree.getProp .share)
                |> Expect.equal (Just 60)
                |> asTest "should distribute share across two nodes"
            , Tree.demoTree
                |> Tree.distributeShare 5 40
                |> Tree.findNodes [ 8, 9 ]
                |> List.map (Maybe.map (Tree.getProp .share))
                |> Expect.equal [ Just 30, Just 30 ]
                |> asTest "should distribute share across three nodes"
            ]
        , describe "getParent"
            [ Tree.demoTree
                |> Tree.getParent 8
                |> Maybe.map (Tree.getProp .label)
                |> Expect.equal (Just "Après vente")
                |> asTest "should find the parent of a given node"
            , Tree.demoTree
                |> Tree.getParent 1
                |> Expect.equal Nothing
                |> asTest "should not find any parent for root"
            ]
        , describe "getSiblings"
            [ Tree.demoTree
                |> Tree.getSiblings 5
                |> List.map (Tree.getProp .id)
                |> Expect.equal [ 8, 9 ]
                |> asTest "should retrieve node siblings across the tree"
            ]
        ]
