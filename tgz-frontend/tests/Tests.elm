module Tests exposing (all)

import ApiTypes exposing (God(..), Land(..), Specialist(..), Square(..), decodeMapLayout, decodeSquare)
import Dict
import Expect
import GameCommand exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Parser
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "The Great Zimbabwe Test Suite"
        [ test "decodeSquare" <|
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
        , test "parseEnd should work" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (Parser.run parseEnd "end")
        , test "parseReligionAndCultureCommand3 should work with build-monuments" <|
            \_ ->
                Expect.equal
                    (Ok (Just (BuildMonuments [ ( 1, "a" ) ])))
                    (Parser.run parseReligionAndCultureCommand3 "build-monuments [a1]")
        , test "parseReligionAndCultureCommand3 should work with build-monument" <|
            \_ ->
                Expect.equal
                    (Ok (Just (BuildMonuments [ ( 1, "a" ) ])))
                    (Parser.run parseReligionAndCultureCommand3 "build-monument a1")
        , test "parseEnd" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (Parser.run parseEnd "end")
        , test "parseUseSpecialist should parse a specialist" <|
            \_ ->
                Expect.equal
                    (Ok (Just <| UseSpecialist Nomads))
                    (Parser.run parseUseSpecialist "use-specialist nomads")
        , test "parseReligionAndCultureCommand1 should work with choose-god" <|
            \_ ->
                Expect.equal
                    (Ok (Just <| ChooseGod Obatala))
                    (Parser.run parseReligionAndCultureCommand1 "choose-god obatala")
        , test "parseReligionAndCultureCommand1 should work with choose-specialist" <|
            \_ ->
                Expect.equal
                    (Ok (Just <| ChooseSpecialist Nomads))
                    (Parser.run parseReligionAndCultureCommand1 "choose-specialist nomads")
        , test "parseReligionAndCultureMultiCommand should work with just end" <|
            \_ ->
                let
                    command =
                        "end"

                    expected : ReligionAndCultureMultiCommand
                    expected =
                        { action1 = Nothing
                        , action2 = Nothing
                        , action3 = Nothing
                        , end = True
                        }
                in
                Expect.equal
                    (Ok expected)
                    (Parser.run parseReligionAndCultureMultiCommand command)
        , test "parseReligionAndCultureMultiCommand should work without use-specialist and choose-god" <|
            \_ ->
                let
                    command =
                        String.join "\n"
                            [ "build-monument a1"
                            , "end"
                            ]

                    expected : ReligionAndCultureMultiCommand
                    expected =
                        { action1 = Nothing
                        , action2 = Nothing
                        , action3 = Just <| BuildMonuments [ ( 1, "a" ) ]
                        , end = True
                        }
                in
                Expect.equal
                    (Ok expected)
                    (Parser.run parseReligionAndCultureMultiCommand command)
        , test "parseReligionAndCultureMultiCommand should work without use-specialist" <|
            \_ ->
                let
                    command =
                        String.join "\n"
                            [ "choose-god obatala"
                            , "build-monument a1"
                            , "end"
                            ]

                    expected : ReligionAndCultureMultiCommand
                    expected =
                        { action1 = Just <| ChooseGod Obatala
                        , action2 = Nothing
                        , action3 = Just <| BuildMonuments [ ( 1, "a" ) ]
                        , end = True
                        }
                in
                Expect.equal
                    (Ok expected)
                    (Parser.run parseReligionAndCultureMultiCommand command)
        , test "parseReligionAndCultureMultiCommand should work with all commands" <|
            \_ ->
                let
                    command =
                        String.join "\n"
                            [ "choose-god obatala"
                            , "use-specialist nomads"
                            , "build-monument a1"
                            , "end"
                            ]

                    expected : ReligionAndCultureMultiCommand
                    expected =
                        { action1 = Just <| ChooseGod Obatala
                        , action2 = Just <| UseSpecialist Nomads
                        , action3 = Just <| BuildMonuments [ ( 1, "a" ) ]
                        , end = True
                        }
                in
                Expect.equal
                    (Ok expected)
                    (Parser.run parseReligionAndCultureMultiCommand command)
        ]
