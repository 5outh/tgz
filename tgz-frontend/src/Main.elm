module Main exposing (Fetch(..), Model, Msg(..), getGame, init, listPlayers, main, update, view)

import ApiTypes as ApiTypes exposing (Empire(..), GameView, MapLayout, Player, decodeGameView, encodeMapLayout, showEmpire)
import Browser
import Browser.Navigation as Nav
import Canvas
import CanvasColor as Color exposing (Color)
import Dict
import Html exposing (Html, canvas, div, h1, h3, img, li, p, text, ul)
import Html.Attributes exposing (height, id, src, style, width)
import Html.Keyed exposing (node)
import Http
import Ports
import Task
import Url
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string)



---- ROUTING ----


type Route
    = Game Int
    | GamePlayer Int String


routes : Parser (Route -> a) a
routes =
    oneOf
        [ map Game (s "game" </> int)
        , map GamePlayer (s "game" </> int </> s "username" </> string)
        ]



---- MODEL ----


type alias Model =
    { game : Fetch Http.Error GameView
    , route : Maybe Route
    , key : Nav.Key
    , url : Url.Url
    }


getGame gameId =
    Http.get
        ("http://localhost:8000/game/" ++ String.fromInt gameId)
        decodeGameView



-- TODO: Apparently this is bad?


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        parsedRoute =
            parse routes url
    in
    ( { game = Loading
      , route = parsedRoute
      , url = url
      , key = key
      }
    , case parsedRoute of
        Nothing ->
            Cmd.none

        Just (Game gameId) ->
            Http.send GotGame (getGame gameId)

        Just (GamePlayer gameId playerId) ->
            Http.send GotGame (getGame gameId)
    )



---- UPDATE ----


type Fetch err a
    = Failure err
    | Loading
    | Success a


type Msg
    = GotGame (Result Http.Error GameView)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        GotGame result ->
            case result of
                Err err ->
                    ( { model | game = Failure err }, Cmd.none )

                Ok gameView ->
                    ( { model | game = Success gameView }
                    , Ports.renderMapLayout (encodeMapLayout gameView.state.mapLayout)
                    )



---- VIEW ----


gameCanvas : Html Msg
gameCanvas =
    node "canvas" [ id "game-canvas", height 720, width 720 ] []


view : Model -> Browser.Document Msg
view model =
    let
        emptyDiv title =
            div []
                [ h1 [] [ text title ]
                , gameCanvas
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
    Browser.Document "The Great Zimbabwe" [ innards ]


renderGame : GameView -> Html Msg
renderGame game =
    div []
        [ div []
            [ h1 [] [ text game.name ]
            ]
        , gameCanvas
        , div [] [ listPlayers (Dict.values game.state.players) ]

        -- TODO: Only if Pre-Setup phase, and add choices
        , renderPreSetupActionBoard game
        ]


listPlayers : List Player -> Html Msg
listPlayers players =
    ul [] (List.map renderPlayer players)


renderPlayer : Player -> Html Msg
renderPlayer player =
    let
        playerEmpire =
            case player.empire of
                Nothing ->
                    "N/A"

                Just empire ->
                    ApiTypes.showEmpire empire

        playerGod =
            case player.god of
                Nothing ->
                    "N/A"

                Just god ->
                    ApiTypes.showGod god
    in
    li []
        [ div []
            [ h3 [] [ text player.info.username ]
            , p [] [ text ("Empire: " ++ playerEmpire) ]
            , p [] [ text ("God: " ++ playerGod) ]
            , p [] [ text ("VR: " ++ String.fromInt player.victoryRequirement) ]
            , p [] [ text ("VP: " ++ String.fromInt player.victoryPoints) ]
            , p [] [ text ("🐄: " ++ String.fromInt player.cattle) ]
            ]
        ]


renderPreSetupActionBoard : GameView -> Html Msg
renderPreSetupActionBoard game =
    let
        players =
            Dict.values game.state.players

        allEmpires =
            [ Kilwa, Mutapa, Zulu, Lozi, Mapungubwe ]

        catMaybes list =
            case list of
                Nothing :: rest ->
                    catMaybes rest

                (Just x) :: rest ->
                    x :: catMaybes rest

                [] ->
                    []

        takenEmpires =
            catMaybes (List.map .empire players)

        isAvailable x =
            not (List.member x takenEmpires)

        availableEmpires =
            List.filter isAvailable allEmpires

        empireElement empire =
            li [] [ text (showEmpire empire) ]
    in
    ul [] (List.map empireElement availableEmpires)



-- allow user to choose from list of empires
-- only display those that have not yet been chosen
---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
