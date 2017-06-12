module Bag exposing
    ( Bag
    , empty, repeat, insert, remove
    , isEmpty, member, count, size
    , union, intersect, diff
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

# Combine
@docs union, intersect, diff
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

{-| Get the union of two sets. For a value, its two counts are added.
-}
union : Bag comparable -> Bag comparable -> Bag comparable
union b1 b2 =
    let
        one : comparable -> Int -> Dict comparable Int -> Dict comparable Int
        one v n d = Dict.insert v n d
        both : comparable -> Int -> Int -> Dict comparable Int -> Dict comparable Int
        both v n1 n2 d = Dict.insert v (n1 + n2) d
    in
        Bag <| Dict.merge one both one (dict b1) (dict b2) Dict.empty

{-| Get the intersection of two sets. For a value, the lesser of its two counts is taken.
-}
intersect : Bag comparable -> Bag comparable -> Bag comparable
intersect b1 b2 =
    let
        one : comparable -> Int -> Dict comparable Int -> Dict comparable Int
        one _ _ d = d
        both : comparable -> Int -> Int -> Dict comparable Int -> Dict comparable Int
        both v n1 n2 d = Dict.insert v (min n1 n2) d
    in
        Bag <| Dict.merge one both one (dict b1) (dict b2) Dict.empty

{-| Get the difference between of two sets.
For a value, the count of the second is removed from the count of the first.
-}
diff : Bag comparable -> Bag comparable -> Bag comparable
diff b1 b2 =
    let
        left : comparable -> Int -> Dict comparable Int -> Dict comparable Int
        left v n d = Dict.insert v n d
        both : comparable -> Int -> Int -> Dict comparable Int -> Dict comparable Int
        both v n1 n2 d =
            if n1 - n2 <= 0
            then d
            else Dict.insert v (n1 - n2) d
        right : comparable -> Int -> Dict comparable Int -> Dict comparable Int
        right _ _ d = d
    in
        Bag <| Dict.merge left both right (dict b1) (dict b2) Dict.empty
