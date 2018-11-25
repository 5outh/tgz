module Main exposing (Fetch(..), Model, Msg(..), getGame, init, listPlayers, main, update, view)

import ApiTypes as ApiTypes exposing (GameView, MapLayout, Player, decodeGameView, encodeMapLayout, showEmpire)
import Browser
import Canvas
import CanvasColor as Color exposing (Color)
import Dict
import Html exposing (Html, canvas, div, h1, h3, img, li, p, text, ul)
import Html.Attributes exposing (height, id, src, style, width)
import Html.Keyed exposing (node)
import Http
import Ports
import Task



---- MODEL ----


type alias Model =
    { game : Fetch Http.Error GameView }


getGame =
    Http.get
        "http://localhost:8000/game/9"
        decodeGameView



-- TODO: Apparently this is bad?


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


init : ( Model, Cmd Msg )
init =
    ( { game = Loading }
    , send Load
      -- Http.send GotGame getGame
    )



---- UPDATE ----


type Fetch err a
    = Failure err
    | Loading
    | Success a


type Msg
    = GotGame (Result Http.Error GameView)
    | Load


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load ->
            ( model, Http.send GotGame getGame )

        GotGame result ->
            case result of
                Err err ->
                    ( { model | game = Failure err }, Cmd.none )

                Ok gameView ->
                    ( { model | game = Success gameView }
                    , Ports.renderMapLayout (encodeMapLayout gameView.state.mapLayout)
                    )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        emptyDiv title =
            div []
                [ h1 [] [ text title ]
                ]

        innards =
            case model.game of
                Loading ->
                    emptyDiv "Loading"

                Failure err ->
                    emptyDiv "Game fetch failed!"

                Success game ->
                    renderGame game
    in
    innards


renderGame : GameView -> Html Msg
renderGame game =
    div []
        [ div []
            [ h1 [] [ text game.name ]
            ]
        , div [] [ listPlayers (Dict.values game.state.players) ]
        ]


listPlayers : List Player -> Html Msg
listPlayers players =
    let
        showPlayerEmpire player =
            case player.empire of
                Nothing ->
                    "N/A"

                Just empire ->
                    ApiTypes.showEmpire empire

        showPlayerGod player =
            case player.god of
                Nothing ->
                    "N/A"

                Just god ->
                    ApiTypes.showGod god

        renderPlayer player =
            li []
                [ div []
                    [ h3 [] [ text player.info.username ]
                    , p [] [ text ("Empire: " ++ showPlayerEmpire player) ]
                    , p [] [ text ("God: " ++ showPlayerGod player) ]
                    , p [] [ text ("VR: " ++ String.fromInt player.victoryRequirement) ]
                    , p [] [ text ("VP: " ++ String.fromInt player.victoryPoints) ]
                    , p [] [ text ("🐄: " ++ String.fromInt player.cattle) ]
                    ]
                ]
    in
    ul [] (List.map renderPlayer players)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
