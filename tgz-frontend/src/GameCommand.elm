module GameCommand exposing
    ( GameCommand(..)
    , encodeGameCommand
    , parseChooseEmpire
    , parseEmpire
    , parseGameCommand
    , parseLocation
    , parsePlaceStartingMonument
    )

import ApiTypes
    exposing
        ( Craftsman
        , Empire(..)
        , God
        , Location
        , Rotated
        , Specialist
        , encodeCraftsman
        , encodeEmpire
        , encodeGod
        , encodeLocation
        , encodeRotated
        , encodeSpecialist
        )
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


type RaiseMonumentCommand
    = UseHub Location
    | UseCraftsman Location Location


type UseSpecialist
    = UseSpecialist Specialist


type ReligionAndCultureCommand1
    = ChooseGod God
    | ChooseSpecialist Specialist


type ReligionAndCultureCommand3
    = BuildMonuments (List Location)
    | PlaceCraftsmen (List ( Location, Rotated Craftsman ))
    | RaiseMonuments (List ( Location, List RaiseMonumentCommand ))


type alias ReligionAndCultureMultiCommand =
    { action1 : Maybe ReligionAndCultureCommand1
    , action2 : Maybe UseSpecialist
    , action3 : Maybe ReligionAndCultureCommand3
    , end : Bool
    }


type GameCommand
    = ChooseEmpire Empire
    | PlaceStartingMonument Location
    | Bid Int
    | Pass
    | ReligionAndCultureCommand ReligionAndCultureMultiCommand


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

                ReligionAndCultureCommand command ->
                    ( "ReligionAndCultureCommand"
                    , encodeValue (encodeReligionAndCultureMultiCommand command)
                    )
    in
    encodeSumObjectWithSingleField keyval val


encodeReligionAndCultureMultiCommand : ReligionAndCultureMultiCommand -> Value
encodeReligionAndCultureMultiCommand val =
    Encode.object
        [ ( "action1", maybeEncode encodeReligionAndCultureCommand1 val.action1 )
        , ( "action2", maybeEncode encodeUseSpecialist val.action2 )
        , ( "action3", maybeEncode encodeReligionAndCultureCommand3 val.action3 )
        , ( "end", Encode.bool val.end )
        ]


encodeReligionAndCultureCommand1 : ReligionAndCultureCommand1 -> Value
encodeReligionAndCultureCommand1 val =
    let
        keyval v =
            case v of
                ChooseGod v1 ->
                    ( "ChooseGod", encodeValue (encodeGod v1) )

                ChooseSpecialist v1 ->
                    ( "ChooseSpecialist", encodeValue (encodeSpecialist v1) )
    in
    encodeSumObjectWithSingleField keyval val


encodeUseSpecialist : UseSpecialist -> Value
encodeUseSpecialist (UseSpecialist v1) =
    encodeSpecialist v1


encodeReligionAndCultureCommand3 : ReligionAndCultureCommand3 -> Value
encodeReligionAndCultureCommand3 val =
    let
        keyval v =
            case v of
                BuildMonuments v1 ->
                    ( "BuildMonuments", encodeValue (Encode.list encodeLocation v1) )

                PlaceCraftsmen v0 ->
                    ( "PlaceCraftsmen"
                    , encodeValue (Encode.list (\( v1, v2 ) -> Encode.list identity [ encodeLocation v1, encodeRotated encodeCraftsman v2 ]) v0)
                    )

                RaiseMonuments v0 ->
                    ( "RaiseMonuments", encodeValue (Encode.list (\( v1, v2 ) -> Encode.list identity [ encodeLocation v1, Encode.list encodeRaiseMonumentCommand v2 ]) v0) )
    in
    encodeSumObjectWithSingleField keyval val


encodeRaiseMonumentCommand : RaiseMonumentCommand -> Value
encodeRaiseMonumentCommand val =
    let
        keyval v =
            case v of
                UseHub v1 ->
                    ( "UseHub", encodeValue (encodeLocation v1) )

                UseCraftsman v1 v2 ->
                    ( "UseCraftsman", encodeValue (Encode.list identity [ encodeLocation v1, encodeLocation v2 ]) )
    in
    encodeSumObjectWithSingleField keyval val
