module GameCommand exposing
    ( GameCommand(..)
    , encodeGameCommand
    , parseChooseEmpire
    , parseEmpire
    , parseGameCommand
    , parseLocation
    , parsePlaceStartingMonument
    )

import ApiTypes exposing (Empire(..), Location, encodeEmpire, encodeLocation)
import Json.Decode
import Json.Encode as Encode exposing (Value)
import Json.Helpers exposing (..)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , chompUntilEndOr
        , float
        , keyword
        , oneOf
        , spaces
        , succeed
        , symbol
        )


type GameCommand
    = ChooseEmpire Empire
    | PlaceStartingMonument Location
    | Bid Int
    | Pass


parseGameCommand : Parser GameCommand
parseGameCommand =
    oneOf
        [ parseChooseEmpire
        , parsePlaceStartingMonument
        , parseBid
        , parsePass
        ]


parseEmpire : Parser Empire
parseEmpire =
    oneOf
        [ succeed Kilwa |. keyword "Kilwa"
        , succeed Mutapa |. keyword "Mutapa"
        , succeed Zulu |. keyword "Zulu"
        , succeed Lozi |. keyword "Lozi"
        , succeed Mapungubwe |. keyword "Mapungubwe"
        ]


parseChooseEmpire : Parser GameCommand
parseChooseEmpire =
    succeed ChooseEmpire
        |. spaces
        |. keyword "choose-empire"
        |. spaces
        |= parseEmpire
        |. chompUntilEndOr "\n"


parseLocation : Parser Location
parseLocation =
    let
        flippedTuple a b =
            ( b, a )

        char =
            oneOf
                (List.map
                    (\x -> succeed (String.fromChar x) |. symbol (String.fromChar x))
                    (String.toList "abcdefghijklmnopqrstuvwxyz")
                )
    in
    succeed flippedTuple
        |= char
        |= Parser.int


parsePlaceStartingMonument : Parser GameCommand
parsePlaceStartingMonument =
    succeed PlaceStartingMonument
        |. spaces
        |. keyword "place-starting-monument"
        |. spaces
        |= parseLocation
        |. chompUntilEndOr "\n"


parseBid : Parser GameCommand
parseBid =
    succeed Bid
        |. spaces
        |. keyword "bid"
        |. spaces
        |= Parser.int
        |. chompUntilEndOr "\n"


parsePass : Parser GameCommand
parsePass =
    succeed Pass
        |. spaces
        |. keyword "pass"
        |. chompUntilEndOr "\n"



-- * Auto-generated


encodeGameCommand : GameCommand -> Value
encodeGameCommand val =
    let
        keyval v =
            case v of
                ChooseEmpire v1 ->
                    ( "ChooseEmpire", encodeValue (encodeEmpire v1) )

                PlaceStartingMonument v1 ->
                    ( "PlaceStartingMonument", encodeValue (encodeLocation v1) )

                Bid amount ->
                    ( "Bid", encodeValue (Encode.int amount) )

                Pass ->
                    ( "Pass", encodeValue (Encode.list (\_ -> Encode.null) []) )
    in
    encodeSumObjectWithSingleField keyval val
