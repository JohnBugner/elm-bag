module Bag exposing
    ( Bag
    , isEmpty, member, count, size
    )

{-| A set of values where, unlike `Set`, each value can appear multiple times.

It's basically just a fancy wrapper for `Dict`, so like `Dict`,
insert, remove, and query operations all take *O(log n)* time.

# Bags
@docs Bag

# Query
@docs isEmpty, member, count, size
-}

import Dict exposing (Dict)

{-| A set of possibly multiple values.
-}
type Bag a
    = Bag (Dict a Int)

{-| Determine if a bag is empty.
-}
isEmpty : Bag a -> Bool
isEmpty (Bag dict) = Dict.isEmpty dict

{-| Determine if a value is in a bag.
-}
member : comparable -> Bag comparable -> Bool
member value (Bag dict) = Dict.member value dict

{-| Determine how many of a certain value are in a bag.
-}
count : comparable -> Bag comparable -> Int
count value (Bag dict) = Maybe.withDefault 0 <| Dict.get value dict

{-| Determine how many values are in a bag.
-}
size : Bag a -> Int
size (Bag dict) = Dict.size dict
