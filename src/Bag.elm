module Bag exposing (Bag)

{-| A set of values where, unlike `Set`, each value can appear multiple times.

It's basically just a fancy wrapper for `Dict`, so like `Dict`,
insert, remove, and query operations all take *O(log n)* time.

# Bags
@docs Bag
-}

import Dict exposing (Dict)

{-| A set of possibly multiple values.
-}
type Bag a
    = Bag (Dict a Int)
