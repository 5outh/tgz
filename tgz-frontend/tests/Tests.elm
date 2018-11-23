module Tests exposing (all)

import ApiTypes exposing (Land(..), Square(..), decodeMapLayout, decodeSquare)
import Dict
import Expect
import Json.Decode as Decode exposing (Decoder)
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \_ ->
                Expect.equal 10 (3 + 7)
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        , test "decodeSquare" <|
            \_ ->
                Expect.equal
                    (Ok (Land BlankLand))
                    (Decode.decodeString decodeSquare "{\"Land\": {\"BlankLand\": []}}")
        , test "decodeMapLayout" <|
            \_ ->
                Expect.equal
                    (Ok (Dict.fromList [ ( ( 0, "a" ), Land BlankLand ) ]))
                    (Decode.decodeString
                        decodeMapLayout
                        "[[{\"x\": 0, \"y\": \"a\"}, {\"Land\": {\"BlankLand\": []}}]]"
                    )
        ]
