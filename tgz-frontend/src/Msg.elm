module Msg exposing (Msg(..))

import ApiTypes exposing (GameView, UserView)
import Browser
import GameCommand exposing (GameCommand(..), encodeGameCommand, parseGameCommand)
import Http
import Url


type Msg
    = Noop
    | GotGame (Result Http.Error GameView)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | IssueGameCommand GameCommand
    | PreviewGameCommand GameCommand
    | UpdateCommand String
    | MapLayoutRendered
    | UpdateUsername String
    | UpdatePassword String
    | UpdateEmail String
    | LoginUser
    | GotLogin (Result Http.Error String)
    | SignupUser
    | GotSignup (Result Http.Error String)
    | GetUser
    | GotUser (Result Http.Error UserView)
