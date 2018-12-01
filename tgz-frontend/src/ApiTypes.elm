module ApiTypes exposing
    ( Empire(..)
    , Game
    , GameView
    , God(..)
    , Land(..)
    , Location
    , MapLayout
    , Player
    , PlayerInfo
    , Square(..)
    , UserView
    , arbitraryDict
    , decodeEmpire
    , decodeGame
    , decodeGameView
    , decodeGod
    , decodeLand
    , decodeMapLayout
    , decodePlayer
    , decodePlayerInfo
    , decodeSquare
    , decodeUserView
    , encodeEmpire
    , encodeLocation
    , encodeMapLayout
    , encodeMonuments
    , encodeUserView
    , intDict
    , showEmpire
    , showGod
    )

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
    , mapLayout : Dict Location Square
    , round : Round
    , step : Int
    }


decodeGame : Decoder Game
decodeGame =
    Decode.succeed (\a b c d -> { players = a, mapLayout = b, round = c, step = d })
        |> required "players" (intDict decodePlayer)
        |> required "map_layout" decodeMapLayout
        |> required "round" jsonDecRound
        |> required "step" Decode.int


type alias Points =
    { points : Int
    , step : Int
    }


decodePoints : Decoder Points
decodePoints =
    Decode.succeed (\a b -> { points = a, step = b })
        |> required "points" Decode.int
        |> required "step" Decode.int


type alias Player =
    { info : PlayerInfo
    , victoryRequirement : Points
    , victoryPoints : Points
    , empire : Maybe Empire
    , cattle : Int
    , monuments : Dict Location Int

    --, craftsmen: Dict Location Craftsman TODO
    --, technologyCards: List (TechnologyCard, Int) TODO
    -- ^ note: TechnologyCard isn't comparable, so we have to decode to a list
    --, specialists: List Specialist TODO
    , god : Maybe God
    }


decodePlayer : Decoder Player
decodePlayer =
    Decode.succeed
        (\a b c d e f g ->
            { info = a
            , victoryRequirement = b
            , victoryPoints = c
            , empire = d
            , cattle = e
            , monuments = f
            , god = g
            }
        )
        |> required "info" decodePlayerInfo
        |> required "victory_requirement" decodePoints
        |> required "victory_points" decodePoints
        |> required "empire" (Decode.maybe decodeEmpire)
        |> required "cattle" Decode.int
        |> required "monuments" decodeMonuments
        |> required "god" (Decode.maybe decodeGod)


decodeEmpire : Decoder Empire
decodeEmpire =
    let
        jsonDecDictEmpire =
            Dict.fromList [ ( "Kilwa", Kilwa ), ( "Mutapa", Mutapa ), ( "Zulu", Zulu ), ( "Lozi", Lozi ), ( "Mapungubwe", Mapungubwe ) ]
    in
    decodeSumUnaries "Empire" jsonDecDictEmpire


encodeEmpire : Empire -> Value
encodeEmpire val =
    case val of
        Kilwa ->
            Encode.string "Kilwa"

        Mutapa ->
            Encode.string "Mutapa"

        Zulu ->
            Encode.string "Zulu"

        Lozi ->
            Encode.string "Lozi"

        Mapungubwe ->
            Encode.string "Mapungubwe"


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


type Square
    = Water
    | Land Land


decodeSquare : Decoder Square
decodeSquare =
    let
        jsonDecDictSquare =
            Dict.fromList
                [ ( "Water", Decode.lazy (\_ -> Decode.succeed Water) )
                , ( "Land", Decode.lazy (\_ -> Decode.map Land decodeLand) )
                ]
    in
    decodeSumObjectWithSingleField "Square" jsonDecDictSquare


encodeSquare : Square -> Value
encodeSquare val =
    let
        keyval v =
            case v of
                Water ->
                    ( "Water", encodeValue (Encode.list identity []) )

                Land v1 ->
                    ( "Land", encodeValue (encodeLand v1) )
    in
    encodeSumObjectWithSingleField keyval val


type Land
    = StartingArea
    | Resource Resource
    | BlankLand


decodeLand : Decoder Land
decodeLand =
    let
        jsonDecDictLand =
            Dict.fromList
                [ ( "StartingArea", Decode.lazy (\_ -> Decode.succeed StartingArea) )
                , ( "Resource", Decode.lazy (\_ -> Decode.map Resource decodeResource) )
                , ( "BlankLand", Decode.lazy (\_ -> Decode.succeed BlankLand) )
                ]
    in
    decodeSumObjectWithSingleField "Land" jsonDecDictLand


encodeLand : Land -> Value
encodeLand val =
    let
        keyval v =
            case v of
                StartingArea ->
                    ( "StartingArea", encodeValue (Encode.list identity []) )

                Resource v1 ->
                    ( "Resource", encodeValue (encodeResource v1) )

                BlankLand ->
                    ( "BlankLand", encodeValue (Encode.list identity []) )
    in
    encodeSumObjectWithSingleField keyval val


type Resource
    = Clay
    | Wood
    | Ivory
    | Diamonds


decodeResource : Decoder Resource
decodeResource =
    let
        jsonDecDictResource =
            Dict.fromList [ ( "Clay", Clay ), ( "Wood", Wood ), ( "Ivory", Ivory ), ( "Diamonds", Diamonds ) ]
    in
    decodeSumNullaries "Resource" jsonDecDictResource


encodeResource : Resource -> Value
encodeResource val =
    case val of
        Clay ->
            Encode.string "Clay"

        Wood ->
            Encode.string "Wood"

        Ivory ->
            Encode.string "Ivory"

        Diamonds ->
            Encode.string "Diamonds"


type alias Location =
    ( Int, String )


decodeLocation : Decoder Location
decodeLocation =
    Decode.succeed (\px py -> ( px, py ))
        |> required "x" Decode.int
        |> required "y" Decode.string


encodeLocation : Location -> Value
encodeLocation ( x, y ) =
    Encode.object
        [ ( "x", Encode.int x )
        , ( "y", Encode.string y )
        ]


type alias MapLayout =
    Dict Location Square


decodeMapLayout : Decoder MapLayout
decodeMapLayout =
    Decode.map
        Dict.fromList
        (Decode.list
            (Decode.map2 tuple2
                (Decode.index 0 decodeLocation)
                (Decode.index 1 decodeSquare)
            )
        )


decodeMonuments : Decoder (Dict Location Int)
decodeMonuments =
    Decode.map
        Dict.fromList
        (Decode.list
            (Decode.map2 tuple2
                (Decode.index 0 decodeLocation)
                (Decode.index 1 Decode.int)
            )
        )


encodeMonuments : Dict Location Int -> Value
encodeMonuments monuments =
    Encode.list
        (\( v1, v2 ) -> Encode.list identity [ encodeLocation v1, Encode.int v2 ])
        (Dict.toList monuments)


encodeMapLayout : MapLayout -> Value
encodeMapLayout val =
    Encode.list
        (\( v1, v2 ) -> Encode.list identity [ encodeLocation v1, encodeSquare v2 ])
        (Dict.toList val)



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


type alias UserView =
    { id : Int
    , username : String
    , email : String
    }


decodeUserView : Decode.Decoder UserView
decodeUserView =
    Decode.succeed (\pid pusername pemail -> { id = pid, username = pusername, email = pemail })
        |> required "id" Decode.int
        |> required "username" Decode.string
        |> required "email" Decode.string


encodeUserView : UserView -> Value
encodeUserView val =
    Encode.object
        [ ( "id", Encode.int val.id )
        , ( "username", Encode.string val.username )
        , ( "email", Encode.string val.email )
        ]


type alias Round =
    { players : List Int
    , currentPlayer : Maybe Int
    , usedMarkers : List ( Location, UsedMarker )
    , generosityOfKingsState : GenerosityOfKingsState
    , currentPhase : Maybe Phase
    }


jsonDecRound : Decode.Decoder Round
jsonDecRound =
    Decode.succeed
        (\pplayers pcurrent_player pused_markers pgenerosity_of_kings_state pcurrent_phase ->
            { players = pplayers
            , currentPlayer = pcurrent_player
            , usedMarkers = pused_markers
            , generosityOfKingsState = pgenerosity_of_kings_state
            , currentPhase = pcurrent_phase
            }
        )
        |> required "players" (Decode.list Decode.int)
        |> fnullable "current_player" Decode.int
        |> required "used_markers" (Decode.list (Decode.map2 tuple2 (Decode.index 0 decodeLocation) (Decode.index 1 jsonDecUsedMarker)))
        |> required "generosity_of_kings_state" jsonDecGenerosityOfKingsState
        |> fnullable "current_phase" jsonDecPhase


type Phase
    = PreSetup
    | Setup
    | GenerosityOfKings
    | ReligionAndCulture
    | Revenues
    | LetUsCompareMythologies


jsonDecPhase : Decode.Decoder Phase
jsonDecPhase =
    let
        jsonDecDictPhase =
            Dict.fromList [ ( "PreSetup", PreSetup ), ( "Setup", Setup ), ( "GenerosityOfKings", GenerosityOfKings ), ( "ReligionAndCulture", ReligionAndCulture ), ( "Revenues", Revenues ), ( "LetUsCompareMythologies", LetUsCompareMythologies ) ]
    in
    decodeSumUnaries "Phase" jsonDecDictPhase


type UsedMarker
    = NotUsed
    | Used
    | UsedTwice


jsonDecUsedMarker : Decode.Decoder UsedMarker
jsonDecUsedMarker =
    let
        jsonDecDictUsedMarker =
            Dict.fromList [ ( "NotUsed", NotUsed ), ( "Used", Used ), ( "UsedTwice", UsedTwice ) ]
    in
    decodeSumUnaries "UsedMarker" jsonDecDictUsedMarker


type alias GenerosityOfKingsState =
    { plaques : List Empire
    , cattle_pool : Int
    , last_bid : Maybe Int
    , players_passed : List Int
    }


jsonDecGenerosityOfKingsState : Decode.Decoder GenerosityOfKingsState
jsonDecGenerosityOfKingsState =
    Decode.succeed (\pplaques pcattle_pool plast_bid pplayers_passed -> { plaques = pplaques, cattle_pool = pcattle_pool, last_bid = plast_bid, players_passed = pplayers_passed })
        |> required "plaques" (Decode.list decodeEmpire)
        |> required "cattle_pool" Decode.int
        |> fnullable "last_bid" Decode.int
        |> required "players_passed" (Decode.list Decode.int)
