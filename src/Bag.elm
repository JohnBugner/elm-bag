module Bag exposing
    ( Bag
    , empty, repeat, insert, remove
    , isEmpty, member, count, size
    , union, intersect, diff
    , toList, fromList
    , map, foldl, foldr, filter, partition
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

# Lists
@docs toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition
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
repeat : Int -> comparable -> Bag comparable
repeat n v =
    if n > 0
    then Bag <| Dict.singleton v n
    else empty

{-| Insert n copies of a value into a bag.
If n is negative, then it removes -n copies of the value from the bag.
-}
insert : Int -> comparable -> Bag comparable -> Bag comparable
insert n v b =
    let
        n_ : Int
        n_ = count v b + n
    in
        if n_ > 0
        then Bag <| Dict.insert v n_ (dict b)
        else Bag <| Dict.remove v (dict b)

{-| Remove n copies of a value from a bag.
If n is greater than the numbers of copies that are in the bag, then all copies are simply removed.

    bag = fromList ['a', 'a', 'b']

    remove 3 'a' bag == fromList ['b']

If n is negative, then it inserts -n copies of the value into the bag.
-}
remove : Int -> comparable -> Bag comparable -> Bag comparable
remove n = insert -n

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

{-| Get the union of two bags. For a value, its two counts are added.
-}
union : Bag comparable -> Bag comparable -> Bag comparable
union b1 b2 =
    let
        f : comparable -> Int -> Int -> Dict comparable Int -> Dict comparable Int
        f v n1 n2 d = Dict.insert v (n1 + n2) d
    in
        Bag <| Dict.merge Dict.insert f Dict.insert (dict b1) (dict b2) Dict.empty

{-| Get the intersection of two bags. For a value, the lesser of its two counts is taken.
-}
intersect : Bag comparable -> Bag comparable -> Bag comparable
intersect b1 b2 =
    let
        f : comparable -> Int -> Int -> Dict comparable Int -> Dict comparable Int
        f v n1 n2 d = Dict.insert v (min n1 n2) d
    in
        Bag <| Dict.merge skip f skip (dict b1) (dict b2) Dict.empty

{-| Get the difference between of two bags.
For a value, the count of the second is removed from the count of the first.
-}
diff : Bag comparable -> Bag comparable -> Bag comparable
diff b1 b2 =
    let
        f : comparable -> Int -> Int -> Dict comparable Int -> Dict comparable Int
        f v n1 n2 d =
            if n1 - n2 <= 0
            then d
            else Dict.insert v (n1 - n2) d
    in
        Bag <| Dict.merge Dict.insert f skip (dict b1) (dict b2) Dict.empty

-- Insert nothing.
skip : comparable -> Int -> Dict comparable Int -> Dict comparable Int
skip _ _ d = d

{-| Convert a bag into a list, sorted from lowest to highest.
-}
toList : Bag comparable -> List comparable
toList b = List.concatMap (\ (v, n) -> List.repeat n v) <| Dict.toList (dict b)

{-| Convert a list into a bag.
-}
fromList : List comparable -> Bag comparable
fromList = List.foldl (insert 1) empty

{-| Map a function onto a bag, creating a new bag.
-}
map : (comparable -> comparable2) -> Bag comparable -> Bag comparable2
map f b = fromList <| List.map f <| toList b

{-| Fold over the values in a bag, in order from lowest to highest.
-}
foldl : (comparable -> b -> b) -> b -> Bag comparable -> b
foldl f r b = Dict.foldl (\ v _ r -> f v r) r (dict b)

{-| Fold over the values in a bag, in order from highest to lowest
-}
foldr : (comparable -> b -> b) -> b -> Bag comparable -> b
foldr f r b = Dict.foldr (\ v _ r -> f v r) r (dict b)

{-| Create a new bag consisting only of values which satisfy a predicate.
-}
filter : (comparable -> Bool) -> Bag comparable -> Bag comparable
filter f b = Bag <| Dict.filter (\ v _ -> f v) (dict b)

{-| Create two new bags; the first consisting of values which satisfy a predicate,
the second consisting of values which do not.
-}
partition : (comparable -> Bool) -> Bag comparable -> (Bag comparable, Bag comparable)
partition f b =
    let
        (p1, p2) = Dict.partition (\ v _ -> f v) (dict b)
    in
        (Bag p1, Bag p2)
