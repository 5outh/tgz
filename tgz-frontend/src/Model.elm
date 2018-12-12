module Model exposing (Fetch(..), Model, Route(..), User)

import ApiTypes as ApiTypes
    exposing
        ( Craftsman(..)
        , Empire(..)
        , EmpirePlaque(..)
        , GameView
        , GenerosityOfKingsState
        , Location
        , MapLayout
        , Phase(..)
        , Player
        , Rotated(..)
        , Specialist(..)
        , decodeGameView
        , encodeCraftsman
        , encodeEmpire
        , encodeLocation
        , encodeMapLayout
        , encodeMonuments
        , getRotated
        , showEmpire
        )
import Browser.Navigation as Nav
import GameCommand exposing (GameCommand(..), encodeGameCommand, parseGameCommand)
import GameError exposing (GameError(..), decodeGameErrorFromBadStatusResponse)
import Http
import Url


type Route
    = Game Int
    | GamePlayer Int String
    | Login


type Fetch err a
    = Failure err
    | Loading
    | Success a


type alias User =
    { username : String
    , password : String
    , email : String
    }


type alias Model =
    { game : Fetch Http.Error GameView
    , gameView : Maybe GameView
    , gameError : Maybe GameError
    , signupError : Maybe String
    , playerCommand : ( String, Maybe GameCommand )
    , route : Maybe Route
    , key : Nav.Key
    , url : Url.Url
    , user : Maybe User
    , userLoggedIn : Bool
    }
