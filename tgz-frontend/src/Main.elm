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

import ApiTypes as ApiTypes
    exposing
        ( Empire(..)
        , EmpirePlaque(..)
        , GameView
        , GenerosityOfKingsState
        , Location
        , MapLayout
        , Player
        , decodeGameView
        , encodeEmpire
        , encodeMapLayout
        , encodeMonuments
        , showEmpire
        )
import Browser
import Browser.Navigation as Nav
import Canvas
import CanvasColor as Color exposing (Color)
import Dict
import Extra.Maybe exposing (..)
import GameCommand exposing (GameCommand(..), encodeGameCommand, parseGameCommand)
import GameError exposing (GameError(..), decodeGameErrorFromBadStatusResponse)
import Html exposing (Attribute, Html, button, canvas, div, h1, h2, h3, img, input, li, p, span, text, ul)
import Html.Attributes exposing (height, id, placeholder, src, style, value, width)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Html.Keyed exposing (node)
import Http
import Json.Decode as Json
import Json.Encode as E
import Parser
import Ports
import Task
import Tuple exposing (first, second)
import Url
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string)


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)


unsafeSend : msg -> Cmd msg
unsafeSend msg =
    Task.succeed msg
        |> Task.perform identity



-- Encode a player for use with map overlay


encodePlayerMonuments : Player -> Maybe E.Value
encodePlayerMonuments player =
    case player.empire of
        Just e ->
            Just <|
                E.object
                    [ ( "empire", encodeEmpire e )
                    , ( "monuments", encodeMonuments player.monuments )
                    ]

        Nothing ->
            Nothing


overlayPlayer : Player -> Cmd msg
overlayPlayer player =
    case encodePlayerMonuments player of
        Nothing ->
            Cmd.none

        Just monuments ->
            Ports.overlayPlayerMonuments monuments



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
    | MapLayoutRendered


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

        MapLayoutRendered ->
            ( model
            , case model.gameView of
                Nothing ->
                    Cmd.none

                Just gameView ->
                    Cmd.batch
                        (List.map
                            (\p ->
                                case encodePlayerMonuments p of
                                    Nothing ->
                                        Cmd.none

                                    Just monuments ->
                                        Ports.overlayPlayerMonuments monuments
                            )
                            (Dict.values gameView.state.players)
                        )
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


trace val =
    Debug.log (Debug.toString val) val


lookupCurrentPlayer : GameView -> Maybe Player
lookupCurrentPlayer game =
    case game.state.round.currentPlayer of
        Nothing ->
            Nothing

        Just playerId ->
            Dict.get playerId game.state.players


renderGame : GameView -> Html Msg
renderGame game =
    let
        currentPlayerUsername =
            Maybe.map (.info >> .username) (lookupCurrentPlayer game)
    in
    div []
        [ div []
            [ h1 [] [ text game.name ]
            ]
        , gameCanvas
        , renderGenerosityOfKingsState game
        , div [] [ listPlayers currentPlayerUsername (trace <| Dict.values game.state.players) ]

        -- TODO: Only if Pre-Setup phase, and add choices
        , renderPreSetupActionBoard game
        ]


listPlayers : Maybe String -> List Player -> Html Msg
listPlayers mCurrentPlayerUsername players =
    ul []
        (List.map
            (\player ->
                renderPlayer
                    (Just player.info.username == mCurrentPlayerUsername)
                    player
            )
            players
        )


renderPlayer : Bool -> Player -> Html Msg
renderPlayer isCurrentPlayer player =
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

        ( primary, secondary ) =
            empireColors player.empire

        renderUsername =
            if isCurrentPlayer then
                h3 [ style "color" primary ]
                    [ text (player.info.username ++ " (current player)") ]

            else
                h3 [ style "color" primary ] [ text player.info.username ]
    in
    div [ style "max-width" "400px" ]
        [ div []
            [ renderUsername
            , ul []
                [ li [] [ text ("Empire: " ++ playerEmpire) ]
                , li [] [ text ("God: " ++ playerGod) ]
                , li [] [ text ("VR: " ++ String.fromInt player.victoryRequirement.points) ]
                , li [] [ text ("VP: " ++ String.fromInt player.victoryPoints.points) ]
                , li [] [ text ("🐄: " ++ String.fromInt player.cattle) ]
                ]
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


renderGenerosityOfKingsState : GameView -> Html Msg
renderGenerosityOfKingsState gameView =
    let
        state =
            gameView.state.round.generosityOfKingsState

        plaquesWithCattle totalCattle plaques =
            let
                len =
                    List.length plaques

                allGet =
                    totalCattle // len

                remaining =
                    modBy len totalCattle

                zip xs ys =
                    case ( xs, ys ) of
                        ( x :: xrest, y :: yrest ) ->
                            ( x, y ) :: zip xrest yrest

                        ( [], _ ) ->
                            []

                        ( _, [] ) ->
                            []

                addOneTo n xs =
                    case ( n, xs ) of
                        ( 0, _ ) ->
                            xs

                        ( _, [] ) ->
                            []

                        ( n0, ( x, m ) :: rest ) ->
                            ( x, m + 1 ) :: addOneTo (n0 - 1) rest
            in
            addOneTo remaining (zip plaques (List.repeat (List.length plaques) allGet))

        renderPlaque plaque =
            case plaque of
                ( PlayerPlaque empire, amount ) ->
                    div [] [ text (showEmpire empire ++ " (" ++ String.fromInt amount ++ ")") ]

                ( ShadipinyiPlaque, amount ) ->
                    div [] [ text ("Shadipinyi (God of Drunks) (" ++ String.fromInt amount ++ ")") ]

        passedPlayers =
            mapMaybe (\playerId -> getPlayerById playerId gameView) state.playersPassed

        passedPlayersText =
            "Players passed: "
                ++ String.join ", " (List.map (.info >> .username) passedPlayers)
    in
    div []
        [ h3 [] [ text "Generosity of Kings" ]
        , div [] [ text <| "Minimum bid: " ++ String.fromInt (1 + Maybe.withDefault 0 state.lastBid) ]
        , div [] (List.map renderPlaque (plaquesWithCattle state.cattlePool state.plaques))
        , div [] [ text passedPlayersText ]
        ]


getPlayerById : Int -> GameView -> Maybe Player
getPlayerById playerId gameView =
    Dict.get playerId gameView.state.players


empireColors mEmpire =
    case mEmpire of
        Nothing ->
            ( "black", "white" )

        Just empire ->
            case empire of
                Kilwa ->
                    ( "red", "white" )

                Mutapa ->
                    ( "yellow", "black" )

                Zulu ->
                    ( "green", "white" )

                Mapungubwe ->
                    ( "white", "black" )

                Lozi ->
                    ( "black", "white" )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Ports.mapLayoutRendered (\_ -> MapLayoutRendered)
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
