module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import LruCache


all : Test
all =
    describe "LruCache"
        [ describe "empty"
            [ fuzz int "creates a cache with a given max size" <|
                \size ->
                    LruCache.empty size
                        |> .maximum
                        |> Expect.equal size
            , fuzz int "always produces a cache with no items" <|
                \size ->
                    LruCache.empty size
                        |> LruCache.size
                        |> Expect.equal 0
            ]
        , describe "insert"
            [ test "never exceeds the max size" <|
                \() ->
                    LruCache.empty 2
                        |> LruCache.insert "a" 1
                        |> LruCache.insert "b" 2
                        |> LruCache.insert "c" 3
                        |> LruCache.size
                        |> Expect.equal 2
            , test "removes the least recently inserted item" <|
                \() ->
                    LruCache.empty 2
                        |> LruCache.insert "a" 1
                        |> LruCache.insert "b" 2
                        |> LruCache.insert "c" 3
                        |> LruCache.member "a"
                        |> Expect.equal False
            ]
        , describe "get"
            [ test "updates internal counters so that recently looked up items aren't removed before others" <|
                \() ->
                    let
                        ( cache, entry ) =
                            LruCache.empty 2
                                |> LruCache.insert "a" 1
                                |> LruCache.insert "b" 2
                                |> LruCache.get "a"

                        finalCache =
                            cache
                                |> LruCache.insert "c" 3

                        hasA =
                            finalCache |> LruCache.member "a"

                        hasB =
                            finalCache |> LruCache.member "b"
                    in
                        Expect.equal ( True, False ) ( hasA, hasB )
            ]
        ]
