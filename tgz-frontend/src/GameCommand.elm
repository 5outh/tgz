module GameCommand exposing
    ( GameCommand(..)
    , RaiseMonumentCommand(..)
    , ReligionAndCultureCommand1(..)
    , ReligionAndCultureCommand3(..)
    , ReligionAndCultureMultiCommand
    , UseSpecialist(..)
    , encodeGameCommand
    , encodeRaiseMonumentCommand
    , encodeReligionAndCultureCommand1
    , encodeReligionAndCultureCommand3
    , encodeReligionAndCultureMultiCommand
    , encodeUseSpecialist
    , parseBid
    , parseChooseEmpire
    , parseEmpire
    , parseEnd
    , parseGameCommand
    , parseGod
    , parseLocation
    , parseLocations
    , parsePass
    , parsePlaceStartingMonument
    , parseReligionAndCultureCommand
    , parseReligionAndCultureCommand1
    , parseReligionAndCultureCommand3
    , parseReligionAndCultureMultiCommand
    , parseSpecialist
    , parseUseSpecialist
    )

import ApiTypes
    exposing
        ( Craftsman
        , Empire(..)
        , God(..)
        , Location
        , Rotated(..)
        , Specialist(..)
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
        , parseReligionAndCultureCommand
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


parseReligionAndCultureCommand : Parser GameCommand
parseReligionAndCultureCommand =
    succeed ReligionAndCultureCommand |= parseReligionAndCultureMultiCommand


parseReligionAndCultureMultiCommand : Parser ReligionAndCultureMultiCommand
parseReligionAndCultureMultiCommand =
    succeed ReligionAndCultureMultiCommand
        |= parseReligionAndCultureCommand1
        |= parseUseSpecialist
        |= parseReligionAndCultureCommand3
        |= parseEnd


parseReligionAndCultureCommand1 : Parser (Maybe ReligionAndCultureCommand1)
parseReligionAndCultureCommand1 =
    oneOf
        [ succeed (Just << ChooseGod)
            |. keyword "choose-god"
            |. spaces
            |= parseGod
            |. chompUntilEndOr "\n"
            |. spaces
        , succeed (Just << ChooseSpecialist)
            |. keyword "choose-specialist"
            |. spaces
            |= parseSpecialist
            |. chompUntilEndOr "\n"
            |. spaces
        , succeed Nothing |. spaces
        ]


parseUseSpecialist : Parser (Maybe UseSpecialist)
parseUseSpecialist =
    oneOf
        [ succeed (Just << UseSpecialist)
            |. keyword "use-specialist"
            |. spaces
            |= parseSpecialist
            |. chompUntilEndOr "\n"
            |. spaces
        , succeed Nothing |. spaces
        ]


parseSpecialist : Parser Specialist
parseSpecialist =
    oneOf
        [ succeed Shaman |. keyword "shaman"
        , succeed RainCeremony |. keyword "rain-ceremony"
        , succeed Herd |. keyword "herd"
        , succeed Builder |. keyword "builder"
        , succeed Nomads |. keyword "nomads"
        ]


parseGod : Parser God
parseGod =
    oneOf
        [ succeed Shadipinyi |. keyword "shadipinyi"
        , succeed Elegua |. keyword "elegua"
        , succeed Dziva |. keyword "dziva"
        , succeed Eshu |. keyword "eshu"
        , succeed Gu |. keyword "gu"
        , succeed Obatala |. keyword "obatala"
        , succeed Atete |. keyword "atete"
        , succeed TsuiGoab |. keyword "tsui-goab"
        , succeed Anansi |. keyword "anansi"
        , succeed (Qamata 0) |. keyword "qamata"
        , succeed Engai |. keyword "engai"
        , succeed Xango |. keyword "xango"
        ]


parseReligionAndCultureCommand3 : Parser (Maybe ReligionAndCultureCommand3)
parseReligionAndCultureCommand3 =
    oneOf
        [ succeed (Just << BuildMonuments)
            |. keyword "build-monuments"
            |. spaces
            |= parseLocations
            |. chompUntilEndOr "\n"
            |. spaces
        , succeed (Just << BuildMonuments << List.singleton)
            |. keyword "build-monument"
            |. spaces
            |= parseLocation
            |. chompUntilEndOr "\n"
            |. spaces

        -- , -- PlaceCraftsmen (List ( Location, Rotated Craftsman ))
        -- , -- RaiseMonuments (List ( Location, List RaiseMonumentCommand ))
        , succeed Nothing |. spaces
        ]


parseLocations : Parser (List Location)
parseLocations =
    Parser.sequence
        { start = "["
        , separator = ","
        , end = "]"
        , spaces = spaces
        , item = parseLocation
        , trailing = Parser.Forbidden
        }


parseEnd : Parser Bool
parseEnd =
    oneOf
        [ succeed True |. spaces |. keyword "end" |. chompUntilEndOr "\n"
        , succeed False
        ]



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
