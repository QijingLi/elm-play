module CounterTest exposing (..)

import Counter exposing (..)
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suit : Test
suit =
    describe "Test Counter functions"
        [ describe "Get a counter from a list"
            [ test "empty case" <|
                \_ -> fromList [] |> Expect.equal Dict.empty
            , test "['a', 'a', 'b', 'b', 'c']" <|
                \_ -> fromList [ 'a', 'a', 'b', 'b', 'c' ] |> Expect.equal (Dict.fromList [ ( 'a', 2 ), ( 'b', 2 ), ( 'c', 1 ) ])
            ]
        , describe "Get elements repeating each as many as its count"
            [ test "empty case" <|
                \_ -> elements Dict.empty |> Expect.equal []
            , test "one element" <|
                \_ -> elements (fromList [ 'a' ]) |> Expect.equal [ 'a' ]
            , test "more complex case" <|
                \_ -> elements (fromList [ 'a', 'a', 'b', 'b', 'c' ]) |> Expect.equal [ 'a', 'a', 'b', 'b', 'c' ]
            , test "elements have less than one" <|
                \_ -> elements (Dict.fromList [ ( 'a', 2 ), ( 'b', 0 ), ( 'c', -3 ) ]) |> Expect.equal [ 'a', 'a' ]
            ]
        , describe "Get the n most common elements"
            [ test "empty case" <|
                \_ -> mostCommon 2 Dict.empty |> Expect.equal []
            , test "typical case" <|
                \_ -> mostCommon 1 (fromList [ 'a', 'a', 'b', 'b', 'c' ]) |> Expect.equal [ 'b' ]
            ]
        , describe "add two Counters"
            [ test "typical case" <|
                \_ ->
                    add (Dict.fromList [ ( 'a', 1 ), ( 'b', 2 ) ]) (Dict.fromList [ ( 'b', 1 ), ( 'c', 3 ) ])
                        |> Expect.equal (Dict.fromList [ ( 'a', 1 ), ( 'b', 3 ), ( 'c', 3 ) ])
            ]
        , describe "and two Counters"
            [ test "typical case" <|
                \_ ->
                    and (Dict.fromList [ ( 'a', 1 ), ( 'b', 2 ) ]) (Dict.fromList [ ( 'b', 1 ), ( 'c', 3 ) ])
                        |> Expect.equal (Dict.fromList [ ( 'b', 1 ) ])
            ]
        , describe "or two Counters"
            [ test "typical case" <|
                \_ ->
                    or (Dict.fromList [ ( 'a', 1 ), ( 'b', 2 ) ]) (Dict.fromList [ ( 'b', 1 ), ( 'c', 3 ) ])
                        |> Expect.equal (Dict.fromList [ ( 'a', 1 ), ( 'b', 2 ), ( 'c', 3 ) ])
            ]
        , describe "subtract two Counters"
            [ test "typical case" <|
                \_ ->
                    subtract (Dict.fromList [ ( 'a', 1 ), ( 'b', 2 ) ]) (Dict.fromList [ ( 'b', 1 ), ( 'c', 3 ) ])
                        |> Expect.equal (Dict.fromList [ ( 'a', 1 ), ( 'b', 1 ) ])
            ]
        ]
