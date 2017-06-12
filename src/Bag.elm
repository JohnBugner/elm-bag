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
repeat value amount = Bag <| Dict.singleton value amount

{-| Insert n copies of a value into a bag.
-}
insert : comparable -> Int -> Bag comparable -> Bag comparable
insert value amount bag = Bag <| Dict.insert value (count value bag + amount) (dict bag)

{-| Remove n copies of a value from a bag.
If n is greater than the numbers of copies that are in the bag, then all copies are simply removed.

    bag = fromList ['a', 'a', 'b']

    remove 'a' 3 bag == fromList ['b']
-}
remove : comparable -> Int -> Bag comparable -> Bag comparable
remove value amount bag =
    if count value bag - amount <= 0
    then Bag <| Dict.remove value (dict bag)
    else Bag <| Dict.insert value (count value bag - amount) (dict bag)

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
