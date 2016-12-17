module LruCache
    exposing
        ( LruCache
        , empty
        , insert
        , get
        , toDict
        , size
        , member
        )

{-| A [least recently used cache](https://en.wikipedia.org/wiki/Cache_replacement_policies#Least_Recently_Used_.28LRU.29)
for key/value pairs.

@docs LruCache, empty, insert, get, size, member, toDict
-}

import Dict exposing (Dict)
import Tuple


{-| The LruCache data type. All internals are exposed in case you want to write
your own tooling or custom functionality.
-}
type alias LruCache comparable a =
    { items : Dict comparable ( a, Int )
    , maximum : Int
    , counter : Int
    }


{-| https://github.com/elm-community/list-extra/blob/4.0.0/src/List/Extra.elm#L227-L248
-}
minimumBy : (a -> comparable) -> List a -> Maybe a
minimumBy f ls =
    let
        minBy x ( y, fy ) =
            let
                fx =
                    f x
            in
                if fx < fy then
                    ( x, fx )
                else
                    ( y, fy )
    in
        case ls of
            [ l_ ] ->
                Just l_

            l_ :: ls_ ->
                Just <| Tuple.first <| List.foldl minBy ( l_, f l_ ) ls_

            _ ->
                Nothing


{-| Create an empty LruCache with a given maximum size

    LruCache.empty 5
-}
empty : Int -> LruCache comparable a
empty maximum =
    { items = Dict.empty
    , maximum = maximum
    , counter = 0
    }


{-| Insert a value into the cache, removing an old value if there isn't enough
room

    -- Will only contain ("b", 2) and ("c", 3)
    LruCache.empty 2
        |> LruCache.insert "a" 1
        |> LruCache.insert "b" 2
        |> LruCache.insert "c" 3
-}
insert : comparable -> a -> LruCache comparable a -> LruCache comparable a
insert key value cache =
    let
        nextItems =
            if cache.counter >= cache.maximum then
                let
                    keyToRemove =
                        cache.items
                            |> Dict.toList
                            |> minimumBy (\( k, ( v, c ) ) -> c)
                            |> Maybe.map Tuple.first
                in
                    case keyToRemove of
                        Just removedKey ->
                            cache.items
                                |> Dict.insert key ( value, cache.counter )
                                |> Dict.remove removedKey

                        Nothing ->
                            cache.items
                                |> Dict.insert key ( value, cache.counter )
            else
                cache.items
                    |> Dict.insert key ( value, cache.counter )
    in
        { cache
            | items = nextItems
            , counter = cache.counter + 1
        }


{-| Looks up a value from the cache and updates the cache to record that the
looked up value was recently used

    initialCache =
        LruCache.empty 2
            |> LruCache.insert "a" 1
            |> LruCache.insert "b" 2

    (nextCache, value) =
        LruCache.get "a" initialCache

    -- Will contain ("a", 1) ("c", 3) because "a" was used last and "b" becomes
    -- the least recently used item
    finalCache =
        nextCache
            |> LruCache.insert "c" 3
-}
get : comparable -> LruCache comparable a -> ( LruCache comparable a, Maybe a )
get key cache =
    case Dict.get key cache.items of
        Just ( entry, counter ) ->
            ( { cache
                | items =
                    Dict.insert key ( entry, cache.counter ) cache.items
                , counter =
                    cache.counter + 1
              }
            , Just entry
            )

        Nothing ->
            ( cache, Nothing )


{-| Gets number of items in the cache

    -- size is 2
    LruCache.empty 2
        |> LruCache.insert "a" 1
        |> LruCache.insert "b" 2
        |> LruCache.size
-}
size : LruCache comparable a -> Int
size cache =
    cache.items
        |> Dict.size


{-| Figures out if a key is in the cache without changing its recency

    -- True
    LruCache.empty 2
        |> LruCache.insert "a" 1
        |> LruCache.insert "b" 2
        |> LruCache.member "b"
-}
member : comparable -> LruCache comparable a -> Bool
member key cache =
    cache.items
        |> Dict.member key


{-| Converts the items in the cache to a Dict
-}
toDict : LruCache comparable a -> Dict comparable a
toDict cache =
    cache.items
        |> Dict.map (\k v -> Tuple.first v)
