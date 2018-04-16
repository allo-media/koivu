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
            [ describe "No lock"
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
            , describe "Locked node"
                [ describe "Two siblings"
                    [ Tree.demoTree
                        |> Tree.toggleLock 5
                        |> Tree.distributeShare 8 22
                        |> Tree.findNodes [ 5, 8, 9 ]
                        |> List.map (Maybe.map (Tree.getProp .share))
                        |> Expect.equal ([ Just 34, Just 22, Just 44 ])
                        |> asTest "should distribute share handling a locked node and two siblings"
                    ]
                , describe "Three siblings"
                    [ Tree.demoTree
                        |> Tree.toggleLock 7
                        |> Tree.distributeShare 6 15
                        |> Tree.findNodes [ 2, 4, 6, 7 ]
                        |> List.map (Maybe.map (Tree.getProp .share))
                        |> Expect.equal ([ Just 30, Just 30, Just 15, Just 25 ])
                        |> asTest "should distribute share handling a locked node and three siblings"
                    ]
                ]
            ]
        , describe "getMaxSharable"
            [ Tree.demoTree
                |> Tree.toggleLock 5
                |> Tree.getMaxSharable 8
                |> Expect.equal 65
                |> asTest "should compute maximum sharable value for node"
            , Tree.demoTree
                |> Tree.toggleLock 8
                |> Tree.getMaxSharable 5
                |> Expect.equal 66
                |> asTest "should compute maximum sharable value for a sibling"
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
