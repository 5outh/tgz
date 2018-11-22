module ApiTypes exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Location  =
   { x: Int
   , y: Char
   }

jsonDecLocation : Json.Decode.Decoder ( Location )
jsonDecLocation =
   Json.Decode.succeed (\px py -> {x = px, y = py})
   |> required "x" (Json.Decode.int)
   |> required "y" (jsonDecChar)

jsonEncLocation : Location -> Value
jsonEncLocation  val =
   Json.Encode.object
   [ ("x", Json.Encode.int val.x)
   , ("y", jsonEncChar val.y)
   ]



type alias Username  = String

jsonDecUsername : Json.Decode.Decoder ( Username )
jsonDecUsername =
    Json.Decode.string

jsonEncUsername : Username -> Value
jsonEncUsername  val = Json.Encode.string val



type alias PlayerInfo  =
   { username: Username
   , email: String
   }

jsonDecPlayerInfo : Json.Decode.Decoder ( PlayerInfo )
jsonDecPlayerInfo =
   Json.Decode.succeed (\pusername pemail -> {username = pusername, email = pemail})
   |> required "username" (jsonDecUsername)
   |> required "email" (Json.Decode.string)

jsonEncPlayerInfo : PlayerInfo -> Value
jsonEncPlayerInfo  val =
   Json.Encode.object
   [ ("username", jsonEncUsername val.username)
   , ("email", Json.Encode.string val.email)
   ]



type Empire  =
    Kilwa 
    | Mutapa 
    | Zulu 
    | Lozi 
    | Mapungubwe 

jsonDecEmpire : Json.Decode.Decoder ( Empire )
jsonDecEmpire = 
    let jsonDecDictEmpire = Dict.fromList [("Kilwa", Kilwa), ("Mutapa", Mutapa), ("Zulu", Zulu), ("Lozi", Lozi), ("Mapungubwe", Mapungubwe)]
    in  decodeSumUnaries "Empire" jsonDecDictEmpire

jsonEncEmpire : Empire -> Value
jsonEncEmpire  val =
    case val of
        Kilwa -> Json.Encode.string "Kilwa"
        Mutapa -> Json.Encode.string "Mutapa"
        Zulu -> Json.Encode.string "Zulu"
        Lozi -> Json.Encode.string "Lozi"
        Mapungubwe -> Json.Encode.string "Mapungubwe"



type Resource  =
    Clay 
    | Wood 
    | Ivory 
    | Diamonds 

jsonDecResource : Json.Decode.Decoder ( Resource )
jsonDecResource = 
    let jsonDecDictResource = Dict.fromList [("Clay", Clay), ("Wood", Wood), ("Ivory", Ivory), ("Diamonds", Diamonds)]
    in  decodeSumUnaries "Resource" jsonDecDictResource

jsonEncResource : Resource -> Value
jsonEncResource  val =
    case val of
        Clay -> Json.Encode.string "Clay"
        Wood -> Json.Encode.string "Wood"
        Ivory -> Json.Encode.string "Ivory"
        Diamonds -> Json.Encode.string "Diamonds"



type Land  =
    StartingArea 
    | Resource Resource
    | BlankLand 

jsonDecLand : Json.Decode.Decoder ( Land )
jsonDecLand =
    let jsonDecDictLand = Dict.fromList
            [ ("StartingArea", Json.Decode.lazy (\_ -> Json.Decode.succeed StartingArea))
            , ("Resource", Json.Decode.lazy (\_ -> Json.Decode.map Resource (jsonDecResource)))
            , ("BlankLand", Json.Decode.lazy (\_ -> Json.Decode.succeed BlankLand))
            ]
    in  decodeSumObjectWithSingleField  "Land" jsonDecDictLand

jsonEncLand : Land -> Value
jsonEncLand  val =
    let keyval v = case v of
                    StartingArea  -> ("StartingArea", encodeValue (Json.Encode.list identity []))
                    Resource v1 -> ("Resource", encodeValue (jsonEncResource v1))
                    BlankLand  -> ("BlankLand", encodeValue (Json.Encode.list identity []))
    in encodeSumObjectWithSingleField keyval val



type Square  =
    Water 
    | Land Land

jsonDecSquare : Json.Decode.Decoder ( Square )
jsonDecSquare =
    let jsonDecDictSquare = Dict.fromList
            [ ("Water", Json.Decode.lazy (\_ -> Json.Decode.succeed Water))
            , ("Land", Json.Decode.lazy (\_ -> Json.Decode.map Land (jsonDecLand)))
            ]
    in  decodeSumObjectWithSingleField  "Square" jsonDecDictSquare

jsonEncSquare : Square -> Value
jsonEncSquare  val =
    let keyval v = case v of
                    Water  -> ("Water", encodeValue (Json.Encode.list identity []))
                    Land v1 -> ("Land", encodeValue (jsonEncLand v1))
    in encodeSumObjectWithSingleField keyval val



type alias MapLayout  = (List (Location, Square))

jsonDecMapLayout : Json.Decode.Decoder ( MapLayout )
jsonDecMapLayout =
    Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecLocation)) (Json.Decode.index 1 (jsonDecSquare)))

jsonEncMapLayout : MapLayout -> Value
jsonEncMapLayout  val = (Json.Encode.list (\(v1,v2) -> Json.Encode.list identity [(jsonEncLocation) v1,(jsonEncSquare) v2])) val



type Craftsman  =
    Potter 
    | IvoryCarver 
    | WoodCarver 
    | DiamondCutter 
    | VesselMaker 
    | ThroneMaker 
    | Sculptor 

jsonDecCraftsman : Json.Decode.Decoder ( Craftsman )
jsonDecCraftsman = 
    let jsonDecDictCraftsman = Dict.fromList [("Potter", Potter), ("IvoryCarver", IvoryCarver), ("WoodCarver", WoodCarver), ("DiamondCutter", DiamondCutter), ("VesselMaker", VesselMaker), ("ThroneMaker", ThroneMaker), ("Sculptor", Sculptor)]
    in  decodeSumUnaries "Craftsman" jsonDecDictCraftsman

jsonEncCraftsman : Craftsman -> Value
jsonEncCraftsman  val =
    case val of
        Potter -> Json.Encode.string "Potter"
        IvoryCarver -> Json.Encode.string "IvoryCarver"
        WoodCarver -> Json.Encode.string "WoodCarver"
        DiamondCutter -> Json.Encode.string "DiamondCutter"
        VesselMaker -> Json.Encode.string "VesselMaker"
        ThroneMaker -> Json.Encode.string "ThroneMaker"
        Sculptor -> Json.Encode.string "Sculptor"



type PrimaryOrSecondary  =
    Primary 
    | Secondary 

jsonDecPrimaryOrSecondary : Json.Decode.Decoder ( PrimaryOrSecondary )
jsonDecPrimaryOrSecondary = 
    let jsonDecDictPrimaryOrSecondary = Dict.fromList [("Primary", Primary), ("Secondary", Secondary)]
    in  decodeSumUnaries "PrimaryOrSecondary" jsonDecDictPrimaryOrSecondary

jsonEncPrimaryOrSecondary : PrimaryOrSecondary -> Value
jsonEncPrimaryOrSecondary  val =
    case val of
        Primary -> Json.Encode.string "Primary"
        Secondary -> Json.Encode.string "Secondary"



type alias TechnologyCard  =
   { name: String
   , craftsman_type: Craftsman
   , victory_requirement: Int
   , victory_points: Int
   , cost: Int
   }

jsonDecTechnologyCard : Json.Decode.Decoder ( TechnologyCard )
jsonDecTechnologyCard =
   Json.Decode.succeed (\pname pcraftsman_type pvictory_requirement pvictory_points pcost -> {name = pname, craftsman_type = pcraftsman_type, victory_requirement = pvictory_requirement, victory_points = pvictory_points, cost = pcost})
   |> required "name" (Json.Decode.string)
   |> required "craftsman_type" (jsonDecCraftsman)
   |> required "victory_requirement" (Json.Decode.int)
   |> required "victory_points" (Json.Decode.int)
   |> required "cost" (Json.Decode.int)

jsonEncTechnologyCard : TechnologyCard -> Value
jsonEncTechnologyCard  val =
   Json.Encode.object
   [ ("name", Json.Encode.string val.name)
   , ("craftsman_type", jsonEncCraftsman val.craftsman_type)
   , ("victory_requirement", Json.Encode.int val.victory_requirement)
   , ("victory_points", Json.Encode.int val.victory_points)
   , ("cost", Json.Encode.int val.cost)
   ]



type Specialist  =
    Shaman 
    | RainCeremony 
    | Herd Int
    | Builder Int
    | Nomads 

jsonDecSpecialist : Json.Decode.Decoder ( Specialist )
jsonDecSpecialist =
    let jsonDecDictSpecialist = Dict.fromList
            [ ("Shaman", Json.Decode.lazy (\_ -> Json.Decode.succeed Shaman))
            , ("RainCeremony", Json.Decode.lazy (\_ -> Json.Decode.succeed RainCeremony))
            , ("Herd", Json.Decode.lazy (\_ -> Json.Decode.map Herd (Json.Decode.int)))
            , ("Builder", Json.Decode.lazy (\_ -> Json.Decode.map Builder (Json.Decode.int)))
            , ("Nomads", Json.Decode.lazy (\_ -> Json.Decode.succeed Nomads))
            ]
    in  decodeSumObjectWithSingleField  "Specialist" jsonDecDictSpecialist

jsonEncSpecialist : Specialist -> Value
jsonEncSpecialist  val =
    let keyval v = case v of
                    Shaman  -> ("Shaman", encodeValue (Json.Encode.list identity []))
                    RainCeremony  -> ("RainCeremony", encodeValue (Json.Encode.list identity []))
                    Herd v1 -> ("Herd", encodeValue (Json.Encode.int v1))
                    Builder v1 -> ("Builder", encodeValue (Json.Encode.int v1))
                    Nomads  -> ("Nomads", encodeValue (Json.Encode.list identity []))
    in encodeSumObjectWithSingleField keyval val



type God  =
    Shadipinyi 
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

jsonDecGod : Json.Decode.Decoder ( God )
jsonDecGod =
    let jsonDecDictGod = Dict.fromList
            [ ("Shadipinyi", Json.Decode.lazy (\_ -> Json.Decode.succeed Shadipinyi))
            , ("Elegua", Json.Decode.lazy (\_ -> Json.Decode.succeed Elegua))
            , ("Dziva", Json.Decode.lazy (\_ -> Json.Decode.succeed Dziva))
            , ("Eshu", Json.Decode.lazy (\_ -> Json.Decode.succeed Eshu))
            , ("Gu", Json.Decode.lazy (\_ -> Json.Decode.succeed Gu))
            , ("Obatala", Json.Decode.lazy (\_ -> Json.Decode.succeed Obatala))
            , ("Atete", Json.Decode.lazy (\_ -> Json.Decode.succeed Atete))
            , ("TsuiGoab", Json.Decode.lazy (\_ -> Json.Decode.succeed TsuiGoab))
            , ("Anansi", Json.Decode.lazy (\_ -> Json.Decode.succeed Anansi))
            , ("Qamata", Json.Decode.lazy (\_ -> Json.Decode.map Qamata (Json.Decode.int)))
            , ("Engai", Json.Decode.lazy (\_ -> Json.Decode.succeed Engai))
            , ("Xango", Json.Decode.lazy (\_ -> Json.Decode.succeed Xango))
            ]
    in  decodeSumObjectWithSingleField  "God" jsonDecDictGod

jsonEncGod : God -> Value
jsonEncGod  val =
    let keyval v = case v of
                    Shadipinyi  -> ("Shadipinyi", encodeValue (Json.Encode.list identity []))
                    Elegua  -> ("Elegua", encodeValue (Json.Encode.list identity []))
                    Dziva  -> ("Dziva", encodeValue (Json.Encode.list identity []))
                    Eshu  -> ("Eshu", encodeValue (Json.Encode.list identity []))
                    Gu  -> ("Gu", encodeValue (Json.Encode.list identity []))
                    Obatala  -> ("Obatala", encodeValue (Json.Encode.list identity []))
                    Atete  -> ("Atete", encodeValue (Json.Encode.list identity []))
                    TsuiGoab  -> ("TsuiGoab", encodeValue (Json.Encode.list identity []))
                    Anansi  -> ("Anansi", encodeValue (Json.Encode.list identity []))
                    Qamata v1 -> ("Qamata", encodeValue (Json.Encode.int v1))
                    Engai  -> ("Engai", encodeValue (Json.Encode.list identity []))
                    Xango  -> ("Xango", encodeValue (Json.Encode.list identity []))
    in encodeSumObjectWithSingleField keyval val



type alias Player  =
   { info: (Alt Maybe PlayerInfo)
   , victory_requirement: (Sum Int)
   , victory_points: (Sum Int)
   , empire: (Alt Maybe Empire)
   , cattle: (Sum Int)
   , monuments: (Merge Location (Sum Int))
   , craftsmen: (List (Location, Craftsman))
   , technology_cards: (Merge TechnologyCard (Sum Int))
   , specialists: (List Specialist)
   , god: (Alt Maybe God)
   }

jsonDecPlayer : Json.Decode.Decoder ( Player )
jsonDecPlayer =
   Json.Decode.succeed (\pinfo pvictory_requirement pvictory_points pempire pcattle pmonuments pcraftsmen ptechnology_cards pspecialists pgod -> {info = pinfo, victory_requirement = pvictory_requirement, victory_points = pvictory_points, empire = pempire, cattle = pcattle, monuments = pmonuments, craftsmen = pcraftsmen, technology_cards = ptechnology_cards, specialists = pspecialists, god = pgod})
   |> required "info" (jsonDecAlt (jsonDecMaybe) (jsonDecPlayerInfo))
   |> required "victory_requirement" (jsonDecSum (Json.Decode.int))
   |> required "victory_points" (jsonDecSum (Json.Decode.int))
   |> required "empire" (jsonDecAlt (jsonDecMaybe) (jsonDecEmpire))
   |> required "cattle" (jsonDecSum (Json.Decode.int))
   |> required "monuments" (jsonDecMerge (jsonDecLocation) (jsonDecSum (Json.Decode.int)))
   |> required "craftsmen" (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecLocation)) (Json.Decode.index 1 (jsonDecCraftsman))))
   |> required "technology_cards" (jsonDecMerge (jsonDecTechnologyCard) (jsonDecSum (Json.Decode.int)))
   |> required "specialists" (Json.Decode.list (jsonDecSpecialist))
   |> required "god" (jsonDecAlt (jsonDecMaybe) (jsonDecGod))

jsonEncPlayer : Player -> Value
jsonEncPlayer  val =
   Json.Encode.object
   [ ("info", (jsonEncAlt (jsonEncMaybe) (jsonEncPlayerInfo)) val.info)
   , ("victory_requirement", (jsonEncSum (Json.Encode.int)) val.victory_requirement)
   , ("victory_points", (jsonEncSum (Json.Encode.int)) val.victory_points)
   , ("empire", (jsonEncAlt (jsonEncMaybe) (jsonEncEmpire)) val.empire)
   , ("cattle", (jsonEncSum (Json.Encode.int)) val.cattle)
   , ("monuments", (jsonEncMerge (jsonEncLocation) ((jsonEncSum (Json.Encode.int)))) val.monuments)
   , ("craftsmen", (Json.Encode.list (\(v1,v2) -> Json.Encode.list identity [(jsonEncLocation) v1,(jsonEncCraftsman) v2])) val.craftsmen)
   , ("technology_cards", (jsonEncMerge (jsonEncTechnologyCard) ((jsonEncSum (Json.Encode.int)))) val.technology_cards)
   , ("specialists", (Json.Encode.list jsonEncSpecialist) val.specialists)
   , ("god", (jsonEncAlt (jsonEncMaybe) (jsonEncGod)) val.god)
   ]



type UsedMarker  =
    NotUsed 
    | Used 
    | UsedTwice 

jsonDecUsedMarker : Json.Decode.Decoder ( UsedMarker )
jsonDecUsedMarker = 
    let jsonDecDictUsedMarker = Dict.fromList [("NotUsed", NotUsed), ("Used", Used), ("UsedTwice", UsedTwice)]
    in  decodeSumUnaries "UsedMarker" jsonDecDictUsedMarker

jsonEncUsedMarker : UsedMarker -> Value
jsonEncUsedMarker  val =
    case val of
        NotUsed -> Json.Encode.string "NotUsed"
        Used -> Json.Encode.string "Used"
        UsedTwice -> Json.Encode.string "UsedTwice"



type alias GenerosityOfKingsState  =
   { plaques: (List Empire)
   , cattle_pool: (Sum Int)
   , last_bid: (Last Int)
   , players_passed: (List PlayerId)
   }

jsonDecGenerosityOfKingsState : Json.Decode.Decoder ( GenerosityOfKingsState )
jsonDecGenerosityOfKingsState =
   Json.Decode.succeed (\pplaques pcattle_pool plast_bid pplayers_passed -> {plaques = pplaques, cattle_pool = pcattle_pool, last_bid = plast_bid, players_passed = pplayers_passed})
   |> required "plaques" (Json.Decode.list (jsonDecEmpire))
   |> required "cattle_pool" (jsonDecSum (Json.Decode.int))
   |> required "last_bid" (jsonDecLast (Json.Decode.int))
   |> required "players_passed" (Json.Decode.list (jsonDecPlayerId))

jsonEncGenerosityOfKingsState : GenerosityOfKingsState -> Value
jsonEncGenerosityOfKingsState  val =
   Json.Encode.object
   [ ("plaques", (Json.Encode.list jsonEncEmpire) val.plaques)
   , ("cattle_pool", (jsonEncSum (Json.Encode.int)) val.cattle_pool)
   , ("last_bid", (jsonEncLast (Json.Encode.int)) val.last_bid)
   , ("players_passed", (Json.Encode.list jsonEncPlayerId) val.players_passed)
   ]



type Phase  =
    PreSetup 
    | Setup 
    | GenerosityOfKings 
    | ReligionAndCulture 
    | Revenues 
    | LetUsCompareMythologies 

jsonDecPhase : Json.Decode.Decoder ( Phase )
jsonDecPhase = 
    let jsonDecDictPhase = Dict.fromList [("PreSetup", PreSetup), ("Setup", Setup), ("GenerosityOfKings", GenerosityOfKings), ("ReligionAndCulture", ReligionAndCulture), ("Revenues", Revenues), ("LetUsCompareMythologies", LetUsCompareMythologies)]
    in  decodeSumUnaries "Phase" jsonDecDictPhase

jsonEncPhase : Phase -> Value
jsonEncPhase  val =
    case val of
        PreSetup -> Json.Encode.string "PreSetup"
        Setup -> Json.Encode.string "Setup"
        GenerosityOfKings -> Json.Encode.string "GenerosityOfKings"
        ReligionAndCulture -> Json.Encode.string "ReligionAndCulture"
        Revenues -> Json.Encode.string "Revenues"
        LetUsCompareMythologies -> Json.Encode.string "LetUsCompareMythologies"



type alias Round  =
   { players: (List PlayerId)
   , current_player: (Last PlayerId)
   , used_markers: (Merge Location (Last UsedMarker))
   , generosity_of_kings_state: GenerosityOfKingsState
   , current_phase: (Last Phase)
   }

jsonDecRound : Json.Decode.Decoder ( Round )
jsonDecRound =
   Json.Decode.succeed (\pplayers pcurrent_player pused_markers pgenerosity_of_kings_state pcurrent_phase -> {players = pplayers, current_player = pcurrent_player, used_markers = pused_markers, generosity_of_kings_state = pgenerosity_of_kings_state, current_phase = pcurrent_phase})
   |> required "players" (Json.Decode.list (jsonDecPlayerId))
   |> required "current_player" (jsonDecLast (jsonDecPlayerId))
   |> required "used_markers" (jsonDecMerge (jsonDecLocation) (jsonDecLast (jsonDecUsedMarker)))
   |> required "generosity_of_kings_state" (jsonDecGenerosityOfKingsState)
   |> required "current_phase" (jsonDecLast (jsonDecPhase))

jsonEncRound : Round -> Value
jsonEncRound  val =
   Json.Encode.object
   [ ("players", (Json.Encode.list jsonEncPlayerId) val.players)
   , ("current_player", (jsonEncLast (jsonEncPlayerId)) val.current_player)
   , ("used_markers", (jsonEncMerge (jsonEncLocation) ((jsonEncLast (jsonEncUsedMarker)))) val.used_markers)
   , ("generosity_of_kings_state", jsonEncGenerosityOfKingsState val.generosity_of_kings_state)
   , ("current_phase", (jsonEncLast (jsonEncPhase)) val.current_phase)
   ]



type alias Game  =
   { players: (Merge PlayerId Player)
   , round: Round
   , map_layout: (First MapLayout)
   , craftsmen: (Merge Craftsman (List TechnologyCard))
   , winner: (Alt Maybe PlayerId)
   }

jsonDecGame : Json.Decode.Decoder ( Game )
jsonDecGame =
   Json.Decode.succeed (\pplayers pround pmap_layout pcraftsmen pwinner -> {players = pplayers, round = pround, map_layout = pmap_layout, craftsmen = pcraftsmen, winner = pwinner})
   |> required "players" (jsonDecMerge (jsonDecPlayerId) (jsonDecPlayer))
   |> required "round" (jsonDecRound)
   |> required "map_layout" (jsonDecFirst (jsonDecMapLayout))
   |> required "craftsmen" (jsonDecMerge (jsonDecCraftsman) (Json.Decode.list (jsonDecTechnologyCard)))
   |> required "winner" (jsonDecAlt (jsonDecMaybe) (jsonDecPlayerId))

jsonEncGame : Game -> Value
jsonEncGame  val =
   Json.Encode.object
   [ ("players", (jsonEncMerge (jsonEncPlayerId) (jsonEncPlayer)) val.players)
   , ("round", jsonEncRound val.round)
   , ("map_layout", (jsonEncFirst (jsonEncMapLayout)) val.map_layout)
   , ("craftsmen", (jsonEncMerge (jsonEncCraftsman) ((Json.Encode.list jsonEncTechnologyCard))) val.craftsmen)
   , ("winner", (jsonEncAlt (jsonEncMaybe) (jsonEncPlayerId)) val.winner)
   ]



type alias GameView  =
   { id: GameId
   , name: String
   , initial_state: Game
   }

jsonDecGameView : Json.Decode.Decoder ( GameView )
jsonDecGameView =
   Json.Decode.succeed (\pid pname pinitial_state -> {id = pid, name = pname, initial_state = pinitial_state})
   |> required "id" (jsonDecGameId)
   |> required "name" (Json.Decode.string)
   |> required "initial_state" (jsonDecGame)

jsonEncGameView : GameView -> Value
jsonEncGameView  val =
   Json.Encode.object
   [ ("id", jsonEncGameId val.id)
   , ("name", Json.Encode.string val.name)
   , ("initial_state", jsonEncGame val.initial_state)
   ]

