module Suite exposing (suite)

import Canopy
import Koivu.Tree as Tree exposing (Tree, NodeInfo)
import Expect exposing (Expectation)
import Test exposing (..)


asTest : String -> Expectation -> Test
asTest label expectation =
    expectation |> always |> test label


suite : Test
suite =
    describe "Data.CustomerPath"
        [ describe "distributeShare"
            [ describe "No lock"
                [ demoTree
                    |> Tree.distributeShare 40 11
                    |> Tree.findNode 12
                    |> Maybe.map (Tree.getProp .share)
                    |> Expect.equal (Just 60)
                    |> asTest "should distribute share across two nodes"
                , demoTree
                    |> Tree.distributeShare 40 5
                    |> Tree.findNodes [ 8, 9 ]
                    |> List.map (Tree.getProp .share)
                    |> Expect.equal [ 30, 30 ]
                    |> asTest "should distribute share across three nodes"
                ]
            , describe "Locked node"
                [ describe "Two siblings"
                    [ demoTree
                        |> Tree.toggleLock 5
                        |> Tree.distributeShare 22 8
                        |> Tree.findNodes [ 5, 8, 9 ]
                        |> List.map (Tree.getProp .share)
                        |> Expect.equal ([ 34, 22, 44 ])
                        |> asTest "should distribute share handling a locked node and two siblings"
                    ]
                , describe "Three siblings"
                    [ demoTree
                        |> Tree.toggleLock 7
                        |> Tree.distributeShare 15 6
                        |> Tree.findNodes [ 2, 4, 6, 7 ]
                        |> List.map (Tree.getProp .share)
                        |> Expect.equal ([ 30, 30, 15, 25 ])
                        |> asTest "should distribute share handling a locked node and three siblings"
                    , demoTree
                        |> Tree.toggleLock 7
                        |> Tree.distributeShare 15 7
                        |> Tree.findNodes [ 2, 4, 6, 7 ]
                        |> List.map (Tree.getProp .share)
                        |> Expect.equal ([ 25, 25, 25, 25 ])
                        |> asTest "should reject distribution if the node is locked"
                    ]
                ]
            ]
        , describe "getMaxSharable"
            [ demoTree
                |> Tree.toggleLock 5
                |> Tree.getMaxSharable 8
                |> Expect.equal 65
                |> asTest "should compute maximum sharable value for node"
            , demoTree
                |> Tree.toggleLock 8
                |> Tree.getMaxSharable 5
                |> Expect.equal 66
                |> asTest "should compute maximum sharable value for a sibling"
            ]
        ]


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
