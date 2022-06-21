module QuickSortTest exposing (..)

import Expect
import QuickSort exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "test quick sort" [
        test "empty" <|
            \_ -> quicksort [] |> Expect.equal (List.sort [])
        ,

        test "one element" <|
            \_ -> quicksort [1] |> Expect.equal (List.sort [1])
        ,
        test "regular case" <|
            let a = [9, -1, 1000, 99, -55, 10]
            in \_ -> quicksort a |> Expect.equal (List.sort a)
        ,
        test "duplicated case" <|
            let a = [9, -1, 9, 9, 9, 9, -55, 55, 1000, 99, -55, 10]
            in \_ -> quicksort a |> Expect.equal (List.sort a)
        ,
        test "string type" <|
            let a = ["hello", "world", "am", "tiger", "who is that"]
            in \_ -> quicksort a |> Expect.equal (List.sort a)
    ]

