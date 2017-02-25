module Tests exposing (..)

import Array
import Hello exposing (Turn(..), reversed, update, Msg(..), Model)
import Test exposing (..)
import Expect
import String


all : Test
all =
    describe "Sample Test Suite"
        [ describe "Unit test examples"
            [ test "Addition" <|
                \() ->
                    Expect.equal (3 + 7) 10
            , test "String.left" <|
                \() ->
                    Expect.equal "a" (String.left 1 "abcdefg")
            ]
        , describe "Left-Right sequence tests"
            [ test "reversed" <|
                \() ->
                    Expect.equal [Left, Right, Right] (reversed [Left, Left, Right])
            , test "update upcoming non-empty" <|
                \() ->
                    Expect.equal
                        ( Model
                            [Left, Left, Right, Left, Left, Right]
                            [Right]
                            (Array.fromList [-1, -2, -1, -2, -3, -2])
                        )
                        ( Tuple.first
                            ( update
                                Next
                                ( Model
                                    [Left, Left, Right, Left, Left]
                                    [Right, Right]
                                    (Array.fromList [-1, -2, -1, -2, -3])
                                )
                            )
                        )
            , test "update upcoming empty" <|
                \() ->
                    Expect.equal
                        ( Model
                            [Left, Left, Right, Left, Left, Right, Right, Left]
                            [Left, Left, Right, Right, Left, Right, Right ]
                            (Array.fromList [-1, -2, -1, -2, -3, -2, -1, -2])
                        )
                        ( Tuple.first
                            ( update
                                Next
                                ( Model
                                    [Left, Left, Right, Left, Left, Right, Right]
                                    []
                                    (Array.fromList [-1, -2, -1, -2, -3, -2, -1])
                                )
                            )
                        )

            ]
        ]
