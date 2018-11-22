module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Http
import ApiTypes exposing (GameView, jsonDecGameView)

---- MODEL ----


type alias Model =
    { game : Fetch Http.Error GameView }


getGame =
  Http.get
    "http://localhost:8000/game/2"
    jsonDecGameView

init : ( Model, Cmd Msg )
init =
    ( { game = Loading },
      Http.send GotGame getGame
    )



---- UPDATE ----

type Fetch err a = Failure err | Loading | Success a

type Msg
    = GotGame (Result Http.Error GameView)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    GotGame result -> case result of
        Err err -> ({model| game = Failure (Debug.log "error!" err)}, Cmd.none)
        Ok gameView -> ({model| game = Success gameView}, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
