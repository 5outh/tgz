module ApiTypes exposing
    ( Craftsman(..)
    , Empire(..)
    , EmpirePlaque(..)
    , Game
    , GameView
    , GenerosityOfKingsState
    , God(..)
    , Land(..)
    , Location
    , MapLayout
    , Phase(..)
    , Player
    , PlayerInfo
    , Rotated(..)
    , Specialist(..)
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
    , encodeCraftsman
    , encodeEmpire
    , encodeGod
    , encodeLocation
    , encodeMapLayout
    , encodeMonuments
    , encodeRotated
    , encodeSpecialist
    , encodeUserView
    , getRotated
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
    , round : Round
    , mapLayout : Dict Location Square
    , gods : List God
    , specialists : List Specialist
    , winner : Maybe Int
    , step : Int
    }


decodeGame : Decoder Game
decodeGame =
    Decode.succeed
        (\a b c d e f g -> { players = a, round = b, mapLayout = c, gods = d, specialists = e, winner = f, step = g })
        |> required "players" (intDict decodePlayer)
        |> required "round" decodeRound
        |> required "map_layout" decodeMapLayout
        |> required "gods" (Decode.list decodeGod)
        |> required "specialists" (Decode.list decodeSpecialist)
        |> required "winner" (Decode.nullable Decode.int)
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
    , craftsmen : Dict Location (Rotated Craftsman)

    --, technologyCards: List (TechnologyCard, Int) TODO
    -- ^ note: TechnologyCard isn't comparable, so we have to decode to a list
    , specialists : List ( Specialist, Int )
    , god : Maybe God
    }


decodePlayer : Decoder Player
decodePlayer =
    Decode.succeed
        (\a b c d e f g h i ->
            { info = a
            , victoryRequirement = b
            , victoryPoints = c
            , empire = d
            , cattle = e
            , monuments = f
            , craftsmen = g
            , specialists = h
            , god = i
            }
        )
        |> required "info" decodePlayerInfo
        |> required "victory_requirement" decodePoints
        |> required "victory_points" decodePoints
        |> required "empire" (Decode.maybe decodeEmpire)
        |> required "cattle" Decode.int
        |> required "monuments" decodeMonuments
        |> required "craftsmen" (decodeLocationDict (decodeRotated decodeCraftsman))
        |> required "specialists"
            (Decode.list (Decode.map2 tuple2 (Decode.index 0 decodeSpecialist) (Decode.index 1 Decode.int)))
        |> required "god" (Decode.maybe decodeGod)


decodeEmpire : Decoder Empire
decodeEmpire =
    let
        decodeDictEmpire =
            Dict.fromList [ ( "Kilwa", Kilwa ), ( "Mutapa", Mutapa ), ( "Zulu", Zulu ), ( "Lozi", Lozi ), ( "Mapungubwe", Mapungubwe ) ]
    in
    decodeSumUnaries "Empire" decodeDictEmpire


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
        decodeDictGod =
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
    decodeSumObjectWithSingleField "God" decodeDictGod


encodeGod : God -> Value
encodeGod val =
    let
        keyval v =
            case v of
                Shadipinyi ->
                    ( "Shadipinyi", encodeValue (Encode.list identity []) )

                Elegua ->
                    ( "Elegua", encodeValue (Encode.list identity []) )

                Dziva ->
                    ( "Dziva", encodeValue (Encode.list identity []) )

                Eshu ->
                    ( "Eshu", encodeValue (Encode.list identity []) )

                Gu ->
                    ( "Gu", encodeValue (Encode.list identity []) )

                Obatala ->
                    ( "Obatala", encodeValue (Encode.list identity []) )

                Atete ->
                    ( "Atete", encodeValue (Encode.list identity []) )

                TsuiGoab ->
                    ( "TsuiGoab", encodeValue (Encode.list identity []) )

                Anansi ->
                    ( "Anansi", encodeValue (Encode.list identity []) )

                Qamata v1 ->
                    ( "Qamata", encodeValue (Encode.int v1) )

                Engai ->
                    ( "Engai", encodeValue (Encode.list identity []) )

                Xango ->
                    ( "Xango", encodeValue (Encode.list identity []) )
    in
    encodeSumObjectWithSingleField keyval val


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
        decodeDictSquare =
            Dict.fromList
                [ ( "Water", Decode.lazy (\_ -> Decode.succeed Water) )
                , ( "Land", Decode.lazy (\_ -> Decode.map Land decodeLand) )
                ]
    in
    decodeSumObjectWithSingleField "Square" decodeDictSquare


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
        decodeDictLand =
            Dict.fromList
                [ ( "StartingArea", Decode.lazy (\_ -> Decode.succeed StartingArea) )
                , ( "Resource", Decode.lazy (\_ -> Decode.map Resource decodeResource) )
                , ( "BlankLand", Decode.lazy (\_ -> Decode.succeed BlankLand) )
                ]
    in
    decodeSumObjectWithSingleField "Land" decodeDictLand


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
        decodeDictResource =
            Dict.fromList [ ( "Clay", Clay ), ( "Wood", Wood ), ( "Ivory", Ivory ), ( "Diamonds", Diamonds ) ]
    in
    decodeSumNullaries "Resource" decodeDictResource


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
    decodeLocationDict Decode.int


decodeLocationDict : Decoder a -> Decoder (Dict Location a)
decodeLocationDict decodeA =
    Decode.map
        Dict.fromList
        (Decode.list
            (Decode.map2 tuple2
                (Decode.index 0 decodeLocation)
                (Decode.index 1 decodeA)
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


decodeRound : Decode.Decoder Round
decodeRound =
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
        |> required "used_markers" (Decode.list (Decode.map2 tuple2 (Decode.index 0 decodeLocation) (Decode.index 1 decodeUsedMarker)))
        |> required "generosity_of_kings_state" decodeGenerosityOfKingsState
        |> fnullable "current_phase" decodePhase


type Phase
    = PreSetup
    | Setup
    | GenerosityOfKings
    | ReligionAndCulture
    | Revenues
    | LetUsCompareMythologies


decodePhase : Decode.Decoder Phase
decodePhase =
    let
        decodeDictPhase =
            Dict.fromList [ ( "PreSetup", PreSetup ), ( "Setup", Setup ), ( "GenerosityOfKings", GenerosityOfKings ), ( "ReligionAndCulture", ReligionAndCulture ), ( "Revenues", Revenues ), ( "LetUsCompareMythologies", LetUsCompareMythologies ) ]
    in
    decodeSumUnaries "Phase" decodeDictPhase


type UsedMarker
    = NotUsed
    | Used
    | UsedTwice


decodeUsedMarker : Decode.Decoder UsedMarker
decodeUsedMarker =
    let
        decodeDictUsedMarker =
            Dict.fromList [ ( "NotUsed", NotUsed ), ( "Used", Used ), ( "UsedTwice", UsedTwice ) ]
    in
    decodeSumUnaries "UsedMarker" decodeDictUsedMarker


type EmpirePlaque
    = PlayerPlaque Empire
    | ShadipinyiPlaque


decodeEmpirePlaque : Decoder EmpirePlaque
decodeEmpirePlaque =
    let
        empirePlaqueDict =
            Dict.fromList
                [ ( "ShadipinyiPlaque", Decode.lazy (\_ -> Decode.succeed ShadipinyiPlaque) )
                , ( "PlayerPlaque", Decode.lazy (\_ -> Decode.map PlayerPlaque decodeEmpire) )
                ]
    in
    decodeSumObjectWithSingleField "Square" empirePlaqueDict


type alias GenerosityOfKingsState =
    { plaques : List EmpirePlaque
    , cattlePool : Int
    , lastBid : Maybe Int
    , playersPassed : List Int
    }


decodeGenerosityOfKingsState : Decode.Decoder GenerosityOfKingsState
decodeGenerosityOfKingsState =
    Decode.succeed (\pplaques pcattlePool plastBid pplayersPassed -> { plaques = pplaques, cattlePool = pcattlePool, lastBid = plastBid, playersPassed = pplayersPassed })
        |> required "plaques" (Decode.list decodeEmpirePlaque)
        |> required "cattle_pool" Decode.int
        |> fnullable "last_bid" Decode.int
        |> required "players_passed" (Decode.list Decode.int)



-- Religion and Culture types


type
    Craftsman
    -- * Primary
    = Potter
    | IvoryCarver
    | WoodCarver
    | DiamondCutter
      -- * Secondary
    | VesselMaker
    | ThroneMaker
    | Sculptor


type Specialist
    = Shaman
    | RainCeremony
    | Herd
    | Builder
    | Nomads



-- ^ Pay 2 cattle to ignore zoning restrictions when building a new monument


type Rotated a
    = Rotated a
    | UnRotated a


getRotated : Rotated a -> a
getRotated rotated =
    case rotated of
        Rotated a ->
            a

        UnRotated a ->
            a


encodeRotated : (a -> Value) -> Rotated a -> Value
encodeRotated encodeA val =
    let
        keyval v =
            case v of
                UnRotated v1 ->
                    ( "UnRotated", encodeValue (encodeA v1) )

                Rotated v1 ->
                    ( "Rotated", encodeValue (encodeA v1) )
    in
    encodeSumObjectWithSingleField keyval val


decodeRotated : Decode.Decoder a -> Decode.Decoder (Rotated a)
decodeRotated aDecoder =
    let
        rotatedDict =
            Dict.fromList
                [ ( "UnRotated", Decode.lazy (\_ -> Decode.map UnRotated aDecoder) )
                , ( "Rotated", Decode.lazy (\_ -> Decode.map Rotated aDecoder) )
                ]
    in
    decodeSumObjectWithSingleField "Rotated" rotatedDict


type ReligionAndCultureCommand1
    = ChooseGod God
    | ChooseSpecialist Specialist


decodeReligionAndCultureCommand1 : Decode.Decoder ReligionAndCultureCommand1
decodeReligionAndCultureCommand1 =
    let
        decodeDictReligionAndCultureCommand1 =
            Dict.fromList
                [ ( "ChooseGod", Decode.lazy (\_ -> Decode.map ChooseGod decodeGod) )
                , ( "ChooseSpecialist", Decode.lazy (\_ -> Decode.map ChooseSpecialist decodeSpecialist) )
                ]
    in
    decodeSumObjectWithSingleField "ReligionAndCultureCommand1" decodeDictReligionAndCultureCommand1


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


type UseSpecialist
    = UseShaman
    | UseRainCeremony Location Location
    | UseHerd Int
    | UseBuilder
    | UseNomads



-- TODO: port this over


decodeUseSpecialist : Decode.Decoder UseSpecialist
decodeUseSpecialist =
    let
        decodeDictUseSpecialist =
            Dict.fromList
                [ ( "UseShaman", Decode.lazy (\_ -> Decode.succeed UseShaman) )
                , ( "UseRainCeremony", Decode.lazy (\_ -> Decode.map2 UseRainCeremony (Decode.index 0 decodeLocation) (Decode.index 1 decodeLocation)) )
                , ( "UseHerd", Decode.lazy (\_ -> Decode.map UseHerd Decode.int) )
                , ( "UseBuilder", Decode.lazy (\_ -> Decode.succeed UseBuilder) )
                , ( "UseNomads", Decode.lazy (\_ -> Decode.succeed UseNomads) )
                ]
    in
    decodeSumObjectWithSingleField "UseSpecialist" decodeDictUseSpecialist


encodeUseSpecialist : UseSpecialist -> Value
encodeUseSpecialist val =
    let
        keyval v =
            case v of
                UseShaman ->
                    ( "UseShaman", encodeValue (Encode.list identity []) )

                UseRainCeremony v1 v2 ->
                    ( "UseRainCeremony", encodeValue (Encode.list identity [ encodeLocation v1, encodeLocation v2 ]) )

                UseHerd v1 ->
                    ( "UseHerd", encodeValue (Encode.int v1) )

                UseBuilder ->
                    ( "UseBuilder", encodeValue (Encode.list identity []) )

                UseNomads ->
                    ( "UseNomads", encodeValue (Encode.list identity []) )
    in
    encodeSumObjectWithSingleField keyval val


type ReligionAndCultureCommand3
    = BuildMonuments (List Location)
    | PlaceCraftsmen (List ( Location, Rotated Craftsman ))
    | RaiseMonuments (List ( Location, List RaiseMonumentCommand ))


decodeReligionAndCultureCommand3 : Decode.Decoder ReligionAndCultureCommand3
decodeReligionAndCultureCommand3 =
    let
        decodeDictReligionAndCultureCommand3 =
            Dict.fromList
                [ ( "BuildMonuments", Decode.lazy (\_ -> Decode.map BuildMonuments (Decode.list decodeLocation)) )
                , ( "PlaceCraftsmen", Decode.lazy (\_ -> Decode.map PlaceCraftsmen (Decode.list (Decode.map2 tuple2 (Decode.index 0 decodeLocation) (Decode.index 1 (decodeRotated decodeCraftsman))))) )
                , ( "RaiseMonuments", Decode.lazy (\_ -> Decode.map RaiseMonuments (Decode.list (Decode.map2 tuple2 (Decode.index 0 decodeLocation) (Decode.index 1 (Decode.list decodeRaiseMonumentCommand))))) )
                ]
    in
    decodeSumObjectWithSingleField "ReligionAndCultureCommand3" decodeDictReligionAndCultureCommand3


encodeReligionAndCultureCommand3 : ReligionAndCultureCommand3 -> Value
encodeReligionAndCultureCommand3 val =
    let
        keyval v =
            case v of
                BuildMonuments v1 ->
                    ( "BuildMonuments", encodeValue (Encode.list encodeLocation v1) )

                PlaceCraftsmen v0 ->
                    ( "PlaceCraftsmen", encodeValue (Encode.list (\( v1, v2 ) -> Encode.list identity [ encodeLocation v1, encodeRotated encodeCraftsman v2 ]) v0) )

                RaiseMonuments v0 ->
                    ( "RaiseMonuments", encodeValue (Encode.list (\( v1, v2 ) -> Encode.list identity [ encodeLocation v1, Encode.list encodeRaiseMonumentCommand v2 ]) v0) )
    in
    encodeSumObjectWithSingleField keyval val


type RaiseMonumentCommand
    = UseHub Location
    | UseCraftsman Location Location


decodeRaiseMonumentCommand : Decode.Decoder RaiseMonumentCommand
decodeRaiseMonumentCommand =
    let
        decodeDictRaiseMonumentCommand =
            Dict.fromList
                [ ( "UseHub", Decode.lazy (\_ -> Decode.map UseHub decodeLocation) )
                , ( "UseCraftsman", Decode.lazy (\_ -> Decode.map2 UseCraftsman (Decode.index 0 decodeLocation) (Decode.index 1 decodeLocation)) )
                ]
    in
    decodeSumObjectWithSingleField "RaiseMonumentCommand" decodeDictRaiseMonumentCommand


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


decodeCraftsman : Decode.Decoder Craftsman
decodeCraftsman =
    let
        decodeDictCraftsman =
            Dict.fromList
                [ ( "Potter", Potter )
                , ( "IvoryCarver", IvoryCarver )
                , ( "WoodCarver", WoodCarver )
                , ( "DiamondCutter", DiamondCutter )
                , ( "VesselMaker", VesselMaker )
                , ( "ThroneMaker", ThroneMaker )
                , ( "Sculptor", Sculptor )
                ]
    in
    decodeSumUnaries "Craftsman" decodeDictCraftsman


encodeCraftsman : Craftsman -> Value
encodeCraftsman val =
    case val of
        Potter ->
            Encode.string "Potter"

        IvoryCarver ->
            Encode.string "IvoryCarver"

        WoodCarver ->
            Encode.string "WoodCarver"

        DiamondCutter ->
            Encode.string "DiamondCutter"

        VesselMaker ->
            Encode.string "VesselMaker"

        ThroneMaker ->
            Encode.string "ThroneMaker"

        Sculptor ->
            Encode.string "Sculptor"


decodeSpecialist : Decode.Decoder Specialist
decodeSpecialist =
    let
        decodeDictSpecialist =
            Dict.fromList
                [ ( "Shaman", Shaman )
                , ( "RainCeremony", RainCeremony )
                , ( "Herd", Herd )
                , ( "Builder", Builder )
                , ( "Nomads", Nomads )
                ]
    in
    decodeSumUnaries "Specialist" decodeDictSpecialist


encodeSpecialist : Specialist -> Value
encodeSpecialist val =
    case val of
        Shaman ->
            Encode.string "Shaman"

        RainCeremony ->
            Encode.string "RainCeremony"

        Herd ->
            Encode.string "Herd"

        Builder ->
            Encode.string "Builder"

        Nomads ->
            Encode.string "Nomads"
