module TreeTest exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tree exposing (..)


suite : Test
suite =
    describe "test tree methods"
        [ test "fromList and toInOrderList" <|
            \_ ->
                let
                    list =
                        [ 9, 3, 1, 5, 6, 10, 4, 12, 13, 4 ]
                in
                fromList list |> toInOrderList |> Expect.equal (List.sort list)
        , test "insert a value" <|
            \_ ->
                let
                    list =
                        [ 9, 3, 1, 5, 6, 10, 4, 12, 13, 4 ]
                in
                fromList list |> insert 15 |> toInOrderList |> Expect.equal (15 :: list |> List.sort)
        , test "flatten a tree layer by layer" <|
            \_ ->
                let
                    tr =
                        fromList [ 9, 3, 1, 5, 6, 10, 4, 12, 13, 4 ]

                    -- flatten a tree using toValueAndDepthPairs
                    flattenTree =
                        toValueAndDepthPairs 0 tr
                            |> List.foldl
                                (\( depth, v ) d ->
                                    case Dict.get depth d of
                                        Just ll ->
                                            Dict.insert depth (v :: ll) d

                                        Nothing ->
                                            Dict.insert depth [ v ] d
                                )
                                Dict.empty
                            |> Dict.map (\_ v -> List.sort v)
                in
                flatten 0 tr Dict.empty |> Expect.equal flattenTree
        ]
