module ApiTypes exposing(..)

import Json.Decode as Decode
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Json.Encode as Encode
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)

type alias GameView  =
   { id: Int
   , name: String
   , state: Game
   }

jsonDecGameView : Decoder GameView
jsonDecGameView =
   Decode.succeed (\a b c -> {id = a, name = b, state = c})
   |> required "id" (Decode.int)
   |> required "name" (Decode.string)
   |> required "state" jsonDecGame

type alias Game =
  { players: Dict Int Player
  }

jsonDecGame : Json.Decode.Decoder Game
jsonDecGame =
   Json.Decode.succeed (\a -> {players = a})
   |> required "players" (intDict jsonDecPlayer)

type alias Player =
    { info: PlayerInfo
    , victoryRequirement: Int
    }

jsonDecPlayer : Json.Decode.Decoder Player
jsonDecPlayer =
   Json.Decode.succeed (\a b -> {info = a,  victoryRequirement = b})
   |> required "info" jsonDecPlayerInfo
   |> required "victory_requirement" Json.Decode.int

type alias PlayerInfo =
    { username: String
    , email: String
    }

jsonDecPlayerInfo : Json.Decode.Decoder PlayerInfo
jsonDecPlayerInfo =
   Json.Decode.succeed (\a b -> {username = a, email = b})
   |> required "username" Json.Decode.string
   |> required "email" Json.Decode.string

-- * Helpers

intDict : Decoder a -> Decoder (Dict Int a)
intDict decoder =
  let
      catMaybes list = case list of
        (Nothing::xs) -> catMaybes xs
        (Just x::xs) -> x::catMaybes xs
        [] -> []
      parseTuple (str, val) = case (String.toInt str) of
        Nothing -> Nothing
        Just i -> Just (i, val)
      transform list = Dict.fromList
        (catMaybes (List.map parseTuple list))
  in Decode.map transform (Decode.keyValuePairs decoder)
