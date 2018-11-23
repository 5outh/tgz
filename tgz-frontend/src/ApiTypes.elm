module ApiTypes exposing (Empire(..), Game, GameView, God(..), Player, PlayerInfo, arbitraryDict, decodeEmpire, decodeGame, decodeGameView, decodeGod, decodePlayer, decodePlayerInfo, intDict, showEmpire, showGod)

-- The following module comes from bartavelle/json-helpers

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Json.Helpers exposing (..)
import Set exposing (Set)


type alias GameView =
    { id : Int
    , name : String
    , state : Game
    }


decodeGameView : Decoder GameView
decodeGameView =
    Decode.succeed (\a b c -> { id = a, name = b, state = c })
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "state" decodeGame


type alias Game =
    { players : Dict Int Player
    }


decodeGame : Decoder Game
decodeGame =
    Decode.succeed (\a -> { players = a })
        |> required "players" (intDict decodePlayer)


type alias Player =
    { info : PlayerInfo
    , victoryRequirement : Int
    , victoryPoints : Int
    , empire : Maybe Empire
    , cattle : Int

    --, monuments: Dict Location Int TODO
    --, craftsmen: Dict Location Craftsman TODO
    --, technologyCards: Dict TechnologyCard Int TODO
    --, specialists: List Specialist TODO
    , god : Maybe God
    }


decodePlayer : Decoder Player
decodePlayer =
    Decode.succeed (\a b c d e f -> { info = a, victoryRequirement = b, victoryPoints = c, empire = d, cattle = e, god = f })
        |> required "info" decodePlayerInfo
        |> required "victory_requirement" Decode.int
        |> required "victory_points" Decode.int
        |> required "empire" (Decode.maybe decodeEmpire)
        |> required "cattle" Decode.int
        |> required "god" (Decode.maybe decodeGod)


decodeEmpire : Decoder Empire
decodeEmpire =
    let
        jsonDecDictEmpire =
            Dict.fromList [ ( "Kilwa", Kilwa ), ( "Mutapa", Mutapa ), ( "Zulu", Zulu ), ( "Lozi", Lozi ), ( "Mapungubwe", Mapungubwe ) ]
    in
    decodeSumUnaries "Empire" jsonDecDictEmpire


decodeGod : Decoder God
decodeGod =
    let
        jsonDecDictGod =
            Dict.fromList
                [ ( "Shadipinyi", Decode.lazy (\_ -> Decode.succeed Shadipinyi) )
                , ( "Elegua", Decode.lazy (\_ -> Decode.succeed Elegua) )
                , ( "Dziva", Decode.lazy (\_ -> Decode.succeed Dziva) )
                , ( "Eshu", Decode.lazy (\_ -> Decode.succeed Eshu) )
                , ( "Gu", Decode.lazy (\_ -> Decode.succeed Gu) )
                , ( "Obatala", Decode.lazy (\_ -> Decode.succeed Obatala) )
                , ( "Atete", Decode.lazy (\_ -> Decode.succeed Atete) )
                , ( "TsuiGoab", Decode.lazy (\_ -> Decode.succeed TsuiGoab) )
                , ( "Anansi", Decode.lazy (\_ -> Decode.succeed Anansi) )
                , ( "Qamata", Decode.lazy (\_ -> Decode.map Qamata Decode.int) )
                , ( "Engai", Decode.lazy (\_ -> Decode.succeed Engai) )
                , ( "Xango", Decode.lazy (\_ -> Decode.succeed Xango) )
                ]
    in
    decodeSumObjectWithSingleField "God" jsonDecDictGod


type alias PlayerInfo =
    { username : String
    , email : String
    }


decodePlayerInfo : Decode.Decoder PlayerInfo
decodePlayerInfo =
    Decode.succeed (\a b -> { username = a, email = b })
        |> required "username" Decode.string
        |> required "email" Decode.string


type Empire
    = Kilwa -- Red
    | Mutapa -- Yellow
    | Zulu -- Green
    | Lozi -- Black
    | Mapungubwe -- White


showEmpire : Empire -> String
showEmpire empire =
    case empire of
        Kilwa ->
            "Kilwa"

        Mutapa ->
            "Mutapa"

        Zulu ->
            "Zulu"

        Lozi ->
            "Lozi"

        Mapungubwe ->
            "Mapungubwe"


type God
    = Shadipinyi
    | Elegua
    | Dziva
    | Eshu
    | Gu
    | Obatala
    | Atete
    | TsuiGoab
    | Anansi
    | Qamata Int
    | Engai
    | Xango


showGod : God -> String
showGod god =
    case god of
        Shadipinyi ->
            "Shadipinyi"

        Elegua ->
            "Elegua"

        Dziva ->
            "Dziva"

        Eshu ->
            "Eshu"

        Gu ->
            "Gu"

        Obatala ->
            "Obatala"

        Atete ->
            "Atete"

        TsuiGoab ->
            "TsuiGoab"

        Anansi ->
            "Anansi"

        Qamata cattle ->
            String.concat [ "Qamata (", String.fromInt cattle, ")" ]

        Engai ->
            "Engai"

        Xango ->
            "Xango"



-- * Helpers


intDict : Decoder a -> Decoder (Dict Int a)
intDict =
    arbitraryDict String.toInt


arbitraryDict : (String -> Maybe comparable) -> Decoder b -> Decoder (Dict comparable b)
arbitraryDict readString decoder =
    let
        catMaybes list =
            case list of
                Nothing :: xs ->
                    catMaybes xs

                (Just x) :: xs ->
                    x :: catMaybes xs

                [] ->
                    []

        parseTuple ( str, val ) =
            case readString str of
                Nothing ->
                    Nothing

                Just i ->
                    Just ( i, val )

        transform list =
            Dict.fromList
                (catMaybes (List.map parseTuple list))
    in
    Decode.map transform (Decode.keyValuePairs decoder)
