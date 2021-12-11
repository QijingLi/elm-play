module Counter exposing (..)

{-| Implement Python Dict in Elm.
Docs: <https://docs.python.org/3/library/collections.html#collections.Dict>
-}

import Dict exposing (..)
import List
import Tuple


{-| Get a new counter from a list
-}
fromList : List comparable -> Dict comparable Int
fromList list =
    case list of
        [] ->
            empty

        x :: xs ->
            update x
                (\count ->
                    case count of
                        Just c ->
                            Just (c + 1)

                        Nothing ->
                            Just 1
                )
                (fromList xs)


{-| Get a list of elements repeating each as many times as its count. If an element's count less than one,
then ignore it.
-}
elements : Dict comparable Int -> List comparable
elements counter =
    Dict.foldl (\k v b -> b ++ List.repeat v k) [] counter


{-| Get a list of the n most common elements and their counts from the most common to the leas
-}
mostCommon : Int -> Dict comparable Int -> List comparable
mostCommon n counter =
    counter
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.reverse
        |> List.take n
        |> List.map Tuple.first


{-| Intersection: min(c[x], d[x])
-}
and : Dict comparable Int -> Dict comparable Int -> Dict comparable Int
and a b =
    Dict.merge
        -- Do nothing
        (\_ _ -> identity)
        (\k leftVal rightVal ->
            Dict.insert k
                (if leftVal < rightVal then
                    leftVal

                 else
                    rightVal
                )
        )
        -- Do nothing
        (\_ _ -> identity)
        a
        b
        Dict.empty


{-| Union: max(c[x], d[x])
-}
or : Dict comparable Int -> Dict comparable Int -> Dict comparable Int
or a b =
    Dict.merge
        (\k leftVal -> Dict.insert k leftVal)
        (\k leftVal rightVal ->
            Dict.insert k
                (if leftVal < rightVal then
                    rightVal

                 else
                    leftVal
                )
        )
        (\k rightVal -> Dict.insert k rightVal)
        a
        b
        Dict.empty


{-| Add two counters
-}
add : Dict comparable Int -> Dict comparable Int -> Dict comparable Int
add a b =
    Dict.merge
        (\k leftVal -> Dict.insert k leftVal)
        (\k leftVal rightVal -> Dict.insert k (leftVal + rightVal))
        (\k rightVal -> Dict.insert k rightVal)
        a
        b
        Dict.empty


{-| Subtract one counter from another
-}
subtract : Dict comparable Int -> Dict comparable Int -> Dict comparable Int
subtract a b =
    Dict.merge
        (\k leftVal -> Dict.insert k leftVal)
        (\k leftVal rightVal -> Dict.insert k (leftVal - rightVal))
        -- Do nothing
        (\_ _ -> identity)
        a
        b
        Dict.empty
