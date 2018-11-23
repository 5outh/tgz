module Main exposing (Fetch(..), Model, Msg(..), getGame, init, listPlayers, main, update, view)

import ApiTypes as ApiTypes exposing (GameView, Player, decodeGameView, showEmpire)
import Browser
import Dict
import Html exposing (Html, div, h1, h3, img, li, p, text, ul)
import Html.Attributes exposing (src)
import Http



---- MODEL ----


type alias Model =
    { game : Fetch Http.Error GameView }


getGame =
    Http.get
        "http://localhost:8000/game/2"
        decodeGameView


init : ( Model, Cmd Msg )
init =
    ( { game = Loading }
    , Http.send GotGame getGame
    )



---- UPDATE ----


type Fetch err a
    = Failure err
    | Loading
    | Success a


type Msg
    = GotGame (Result Http.Error GameView)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotGame result ->
            case result of
                Err err ->
                    ( { model | game = Failure (Debug.log "error!" err) }, Cmd.none )

                Ok gameView ->
                    ( { model | game = Success gameView }, Cmd.none )



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
        , listPlayers (Dict.values game.state.players)
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
                    , p [] [ text (String.append "Empire: " (showPlayerEmpire player)) ]
                    , p [] [ text (String.append "God: " (showPlayerGod player)) ]
                    , p [] [ text (String.append "VR: " (String.fromInt player.victoryRequirement)) ]
                    , p [] [ text (String.append "VP: " (String.fromInt player.victoryPoints)) ]
                    , p [] [ text (String.append "Cattle: " (String.fromInt player.cattle)) ]
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
