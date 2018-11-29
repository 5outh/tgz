module GameCommand exposing
    ( GameCommand(..)
    , parseChooseEmpire
    , parseEmpire
    , parseGameCommand
    , parseLocation
    , parsePlaceStartingMonument
    )

import Json.Encode
import ApiTypes exposing (Empire(..), Location)
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


parseGameCommand : Parser GameCommand
parseGameCommand =
    oneOf
        [ parseChooseEmpire
        , parsePlaceStartingMonument
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
