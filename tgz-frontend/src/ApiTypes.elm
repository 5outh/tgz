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
   Decode.succeed (\pid pname pstate-> {id = pid, name = pname, state = pstate})
   |> required "id" (Decode.int)
   |> required "name" (Decode.string)
   |> required "state" jsonDecGame

jsonEncGameView : GameView -> Value
jsonEncGameView  val =
   Encode.object
   [ ("id", Encode.int val.id)
   , ("name", Encode.string val.name)
   ]

type alias Game =
  { players: Dict Int Player
  }

jsonDecGame : Json.Decode.Decoder Game
jsonDecGame =
   Json.Decode.succeed (\pplayers -> {players = pplayers})
   |> required "players" (intDict jsonDecPlayer)

type alias Player =
    { victoryRequirement: Int
    }

jsonDecPlayer : Json.Decode.Decoder Player
jsonDecPlayer =
   Json.Decode.succeed (\pvictoryRequirement -> {victoryRequirement = pvictoryRequirement})
   |> required "victory_requirement" Json.Decode.int

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
