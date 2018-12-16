module Model exposing (Fetch(..), HomePageState, Model, Route(..))

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
import Extra.Maybe exposing (lastMay)
import GameCommand exposing (GameCommand(..), encodeGameCommand, parseGameCommand)
import GameError exposing (GameError(..), decodeGameErrorFromBadStatusResponse)
import Http
import Models.User exposing (User)
import Url


type Route
    = Game Int
    | GamePlayer Int String
    | Login
    | Home String -- Username


type Fetch err a
    = Failure err
    | Loading
    | Success a


type alias HomePageState =
    { games : List GameView
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
    , homePageState : HomePageState
    }



-- TODO: idk if this is necessary


merge : Model -> Model -> Model
merge model1 model2 =
    { game = model2.game
    , gameView = lastMay model1.gameView model2.gameView
    , gameError = model2.gameError
    , signupError = model2.signupError
    , playerCommand = model2.playerCommand
    , route = model2.route
    , key = model2.key
    , url = model2.url
    , user = lastMay model1.user model2.user
    , userLoggedIn = model2.userLoggedIn
    , homePageState = model2.homePageState
    }
