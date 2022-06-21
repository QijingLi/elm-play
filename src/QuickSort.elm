module QuickSort exposing (..)

{-|
 This implementation is from [Learn You a Haskell For Great Good](http://learnyouahaskell.com/recursion)
 which is not efficient but capture the general idea of quicksort.
-}


quicksort : List comparable -> List comparable
quicksort a =
    case a of
        [] ->
            []
        head :: rest ->
            -- partition the rest of list(exclude the head) into two lists by comparing the head
            let ( smaller, bigger ) = List.partition (\e -> e <= head) rest
            in quicksort smaller ++ [ head ] ++ quicksort bigger
