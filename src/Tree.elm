module Tree exposing
    ( flatten
    , fromList
    , insert
    , merged
    , singleton
    , toInOrderList
    , toValueAndDepthPairs
    )

{-| You can create a `Binary Search Tree` with `fromList` or `singleton` and `insert`.
This has a bunch of functions that help to understand how elm work with a tree structure.
-}

import Dict


{-| Define a binary tree node
-}
type Tree a
    = Empty
    | Leaf a
    | Node a (Tree a) (Tree a)


{-| Create a tree with single node
-}
singleton : a -> Tree a
singleton x =
    Leaf x


{-| Insert a value into the tree
-}
insert : comparable -> Tree comparable -> Tree comparable
insert x t =
    case t of
        Empty ->
            Leaf x

        Leaf v ->
            if x < v then
                Node v (Leaf x) Empty

            else
                Node v Empty (Leaf x)

        Node v left right ->
            if x < v then
                Node v (insert x left) right

            else
                Node v left (insert x right)


{-| Build a binary search tree from a list
-}
fromList : List comparable -> Tree comparable
fromList list =
    case list of
        [] ->
            Empty

        head :: _ ->
            let
                -- Partition the list into the smaller part and bigger or equal part by the head element
                ( smaller, bigger ) =
                    List.partition (\x -> x < head) (List.drop 1 list)
            in
            Node head (fromList smaller) (fromList bigger)


{-| Travel a tree in order
-}
toInOrderList : Tree comparable -> List comparable
toInOrderList t =
    case t of
        Empty ->
            []

        Leaf v ->
            [ v ]

        Node v left right ->
            toInOrderList left ++ [ v ] ++ toInOrderList right


{-| Travel a tree and return a list of pairs of a node's value and its depth
-}
toValueAndDepthPairs : Int -> Tree a -> List ( Int, a )
toValueAndDepthPairs depth t =
    case t of
        Empty ->
            []

        Leaf v ->
            [ ( depth, v ) ]

        Node v left right ->
            toValueAndDepthPairs (depth + 1) left ++ [ ( depth, v ) ] ++ toValueAndDepthPairs (depth + 1) right


{-| Extend a list of elements to a dict(comparable: list) when the given key exists, otherwise insert the key and the list.
This is a helper function for flatten.
-}
extend : comparable -> List a -> Dict.Dict comparable (List a) -> Dict.Dict comparable (List a)
extend k v dictA =
    case Dict.get k dictA of
        Just valA ->
            Dict.insert k (valA ++ v) dictA

        Nothing ->
            Dict.insert k v dictA


{-| Merge two dicts
This is a helper function for flatten.
-}
merged : Dict.Dict comparable (List a) -> Dict.Dict comparable (List a) -> Dict.Dict comparable (List a)
merged dictA dictB =
    dictB
        |> Dict.foldl extend dictA


{-| Append a value into the dict(Int->List a) when the key exists otherwise insert a new key=value.
This is a helper function for flatten.
-}
append : comparable -> a -> Dict.Dict comparable (List a) -> Dict.Dict comparable (List a)
append key val dict =
    case Dict.get key dict of
        Just v ->
            Dict.insert key (val :: v) dict

        Nothing ->
            Dict.insert key [ val ] dict


{-| Flatten that breaks a tree into the layers and return them as a dictionary in which
the depth serve as dictionary keys and the nodes of same depth as dictionary values but nodes keep
the same relative order in the tree.
-}
flatten : Int -> Tree a -> Dict.Dict Int (List a) -> Dict.Dict Int (List a)
flatten depth t dict =
    case t of
        Empty ->
            dict

        Leaf v ->
            append depth v dict

        Node v left right ->
            flatten (depth + 1) right dict
                |> merged (flatten (depth + 1) left dict)
                |> merged
                    (append depth v dict)
