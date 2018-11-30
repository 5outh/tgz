module Main exposing
    ( Fetch(..)
    , Model
    , Msg(..)
    , getGame
    , init
    , listPlayers
    , main
    , update
    , view
    )

import ApiTypes as ApiTypes exposing (Empire(..), GameView, MapLayout, Player, decodeGameView, encodeMapLayout, showEmpire)
import Browser
import Browser.Navigation as Nav
import Canvas
import CanvasColor as Color exposing (Color)
import Dict
import GameCommand exposing (GameCommand(..), encodeGameCommand, parseGameCommand)
import GameError exposing (GameError(..), decodeGameErrorFromBadStatusResponse)
import Html exposing (Attribute, Html, button, canvas, div, h1, h3, img, input, li, p, span, text, ul)
import Html.Attributes exposing (height, id, placeholder, src, style, value, width)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Html.Keyed exposing (node)
import Http
import Json.Decode as Json
import Parser
import Ports
import Task
import Tuple exposing (first, second)
import Url
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string)


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)



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
    , gameView : Maybe GameView
    , gameError : Maybe GameError
    , playerCommand : ( String, Maybe GameCommand )
    , route : Maybe Route
    , key : Nav.Key
    , url : Url.Url
    }


getGame gameId =
    Http.get
        ("http://localhost:8000/game/" ++ String.fromInt gameId)
        decodeGameView


issueCommand gameId username command =
    Http.post
        ("http://localhost:8000/game/"
            ++ String.fromInt gameId
            ++ "/player/"
            ++ username
            ++ "/command"
        )
        (Http.jsonBody (encodeGameCommand command))
        decodeGameView


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        parsedRoute =
            parse routes url
    in
    ( { game = Loading
      , gameView = Nothing
      , gameError = Nothing
      , playerCommand = ( "", Nothing )
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
    = Noop
    | GotGame (Result Http.Error GameView)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | IssueGameCommand GameCommand
    | UpdateCommand String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

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
                    ( { model
                        | game = Failure err
                        , gameError = decodeGameErrorFromBadStatusResponse err
                      }
                    , Cmd.none
                    )

                -- Getting an 'OK' response will clear out errors.
                Ok gameView ->
                    ( { model
                        | game = Success gameView
                        , gameView = Just gameView
                        , gameError = Nothing
                      }
                    , Ports.renderMapLayout (encodeMapLayout gameView.state.mapLayout)
                    )

        IssueGameCommand command ->
            case model.route of
                Just (GamePlayer gameId username) ->
                    ( model, Http.send GotGame (issueCommand gameId username command) )

                _ ->
                    ( model, Cmd.none )

        UpdateCommand commandString ->
            case Parser.run parseGameCommand commandString of
                Ok cmd ->
                    ( { model | playerCommand = ( commandString, Just cmd ) }, Cmd.none )

                Err _ ->
                    ( { model | playerCommand = ( commandString, Nothing ) }, Cmd.none )



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

        loadingDiv =
            case model.game of
                Loading ->
                    div [] [ text "Loading game..." ]

                _ ->
                    div [] []

        errorDiv =
            case model.gameError of
                Nothing ->
                    div [] []

                Just gameError ->
                    case gameError of
                        InternalError message ->
                            div []
                                [ text ("Server Error (not your fault!) " ++ message) ]

                        InvalidAction message ->
                            div [] [ text message ]

        -- NOTE: This is extremely finnicky. Be careful modifying this, or
        -- the game may completely disappear from the screen.
        gameDiv =
            case model.gameView of
                Just gameView ->
                    renderGame gameView

                _ ->
                    emptyDiv "Loading..."
    in
    Browser.Document "The Great Zimbabwe" [ loadingDiv, errorDiv, gameDiv, renderControlPanel model ]



-- TODO: refactor to place control panel within


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


renderControlPanel : Model -> Html Msg
renderControlPanel model =
    let
        parsedCommand =
            second model.playerCommand

        issueCommandIfPossible cmd =
            case cmd of
                Nothing ->
                    Noop

                Just gameCommand ->
                    IssueGameCommand gameCommand
    in
    div []
        [ input
            [ placeholder "Enter command"
            , value (first model.playerCommand)
            , onInput UpdateCommand
            , onKeyUp
                (\i ->
                    if i == 13 then
                        issueCommandIfPossible parsedCommand

                    else
                        Noop
                )
            ]
            []
        , div [] [ text <| Debug.toString (second model.playerCommand) ]
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
            div []
                [ button [ onClick (IssueGameCommand (ChooseEmpire empire)) ]
                    [ text (showEmpire empire) ]
                ]
    in
    span [] (List.map empireElement availableEmpires)



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
