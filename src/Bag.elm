module Bag exposing
    ( Bag
    , empty, repeat, insert, remove
    , isEmpty, member, count, size
    )

{-| A set of values where, unlike `Set`, each value can appear multiple times.

It's basically just a fancy wrapper for `Dict`, so like `Dict`,
insert, remove, and query operations all take *O(log n)* time.

# Bags
@docs Bag

# Build
@docs empty, repeat, insert, remove

# Query
@docs isEmpty, member, count, size
-}

import Dict exposing (Dict)

{-| A set of possibly multiple values.
-}
type Bag a
    = Bag (Dict a Int)

dict : Bag a -> Dict a Int
dict (Bag d) = d

{-| Create an empty bag.
-}
empty : Bag a
empty = Bag Dict.empty

{-| Create a bag with n copies of a value.
-}
repeat : comparable -> Int -> Bag comparable
repeat v n = Bag <| Dict.singleton v n

{-| Insert n copies of a value into a bag.
-}
insert : comparable -> Int -> Bag comparable -> Bag comparable
insert v n b = Bag <| Dict.insert v (count v b + n) (dict b)

{-| Remove n copies of a value from a bag.
If n is greater than the numbers of copies that are in the bag, then all copies are simply removed.

    bag = fromList ['a', 'a', 'b']

    remove 'a' 3 bag == fromList ['b']
-}
remove : comparable -> Int -> Bag comparable -> Bag comparable
remove v n b =
    if count v b - n <= 0
    then Bag <| Dict.remove v (dict b)
    else Bag <| Dict.insert v (count v b - n) (dict b)

{-| Determine if a bag is empty.
-}
isEmpty : Bag a -> Bool
isEmpty b = Dict.isEmpty (dict b)

{-| Determine if a value is in a bag.
-}
member : comparable -> Bag comparable -> Bool
member v b = Dict.member v (dict b)

{-| Determine the number of copies of a value in a bag.
-}
count : comparable -> Bag comparable -> Int
count v b = Maybe.withDefault 0 <| Dict.get v (dict b)

{-| Determine the number of values in a bag.
-}
size : Bag a -> Int
size b = Dict.size (dict b)
