module Models exposing (GameId(..), PlayerId(..), Route(..))


type GameId
    = Int


type Username
    = String


type Route
    = GamePlayerRoute Username GameId
