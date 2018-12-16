module Main exposing
    ( getGame
    , init
    , main
    , update
    , view
    )

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
        , UserView
        , decodeGameView
        , encodeCraftsman
        , encodeEmpire
        , encodeLocation
        , encodeMapLayout
        , encodeMonuments
        , getRotated
        , showEmpire
        )
import Base64
import Browser
import Browser.Navigation as Nav
import Canvas
import CanvasColor as Color exposing (Color)
import Dict
import Extra.Cmd as ExtraCmd
import Extra.Maybe exposing (..)
import GameCommand exposing (GameCommand(..), encodeGameCommand, parseGameCommand)
import GameError exposing (GameError(..), decodeGameErrorFromBadStatusResponse)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , canvas
        , div
        , h1
        , h2
        , h3
        , img
        , input
        , li
        , p
        , span
        , text
        , textarea
        , ul
        )
import Html.Attributes exposing (height, href, id, placeholder, src, style, value, width)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Html.Keyed exposing (node)
import Http
import Json.Decode as D
import Json.Encode as E
import Json.Helpers exposing (..)
import Layout
import Model exposing (..)
import Models.User exposing (..)
import Msg exposing (Msg(..))
import Page.Home exposing (home)
import Page.Login exposing (loginPage)
import Parser
import Player exposing (..)
import Ports
import Supply exposing (renderSupply)
import Task
import Tuple exposing (first, second)
import Url
import Url.Builder as Builder
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string)



-- Encode a player for use with map overlay


encodeUsedMarkers : List ( Location, Int ) -> E.Value
encodeUsedMarkers =
    let
        encodeUsedMarker ( location, times ) =
            E.object
                [ ( "location", encodeLocation location )
                , ( "times", E.int times )
                ]
    in
    E.list encodeUsedMarker


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


encodePlayerCraftsmenAndEmpire : Player -> Maybe E.Value
encodePlayerCraftsmenAndEmpire player =
    case player.empire of
        Just e ->
            Just <|
                E.object
                    [ ( "empire", encodeEmpire e )
                    , ( "craftsmen", encodePlayerCraftsmen (Dict.toList player.craftsmen) )
                    ]

        Nothing ->
            Nothing


encodePlayerCraftsmen : List ( Location, Rotated Craftsman ) -> E.Value
encodePlayerCraftsmen =
    let
        encodeDimensions ( w, h ) =
            E.object
                [ ( "width", E.int w )
                , ( "height", E.int h )
                ]

        encodePlayerCraftsman ( location, rotatedCraftsman ) =
            E.object
                [ ( "location", encodeLocation location )
                , ( "craftsman", encodeCraftsman (getRotated rotatedCraftsman) )
                , ( "dimensions", encodeDimensions (Layout.rotatedDimensions rotatedCraftsman) )
                ]
    in
    E.list encodePlayerCraftsman


overlayPlayerCraftsmen : Player -> Cmd msg
overlayPlayerCraftsmen player =
    case encodePlayerCraftsmenAndEmpire player of
        Nothing ->
            Cmd.none

        Just craftsmen ->
            Ports.overlayPlayerCraftsmen craftsmen


overlayPlayerMonuments : Player -> Cmd msg
overlayPlayerMonuments player =
    case encodePlayerMonuments player of
        Nothing ->
            Cmd.none

        Just monuments ->
            Ports.overlayPlayerMonuments monuments


overlayUsedMarkers : List ( Location, Int ) -> Cmd msg
overlayUsedMarkers usedMarkers =
    Ports.overlayUsedMarkers (encodeUsedMarkers usedMarkers)



---- ROUTING ----


routes : Parser (Route -> a) a
routes =
    oneOf
        [ map Game (s "game" </> int)
        , map GamePlayer (s "game" </> int </> s "username" </> string)
        , map Login (s "login")
        , map Home (s "home" </> string)
        ]



---- MODEL ----


authenticatedGet { username, password } url expect =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization"
                ("Basic " ++ Base64.encode (username ++ ":" ++ password))
            ]
        , url = url
        , expect = expect
        , body = Http.emptyBody
        , timeout = Nothing
        , withCredentials = False
        }


unAuthenticatedPost url body expect =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , expect = expect
        , body = body
        , timeout = Nothing
        , withCredentials = False
        }


postSignup user =
    unAuthenticatedPost
        "http://localhost:8000/signup"
        (Http.jsonBody <| encodeUser user)
        Http.expectString


postLogin user =
    unAuthenticatedPost
        "http://localhost:8000/login"
        (Http.jsonBody <| encodeUser user)
        Http.expectString


getGame user gameId =
    authenticatedGet
        user
        ("http://localhost:8000/game/" ++ String.fromInt gameId)
        (Http.expectJson decodeGameView)


getUser : User -> String -> Http.Request UserView
getUser user username =
    authenticatedGet
        user
        ("http://localhost:8000/users/" ++ username)
        (Http.expectJson ApiTypes.decodeUserView)


issueCommand gameId username command =
    unAuthenticatedPost
        -- TODO: Authenticate, pass user
        ("http://localhost:8000/game/"
            ++ String.fromInt gameId
            ++ "/player/"
            ++ username
            ++ "/command"
        )
        (Http.jsonBody (encodeGameCommand command))
        (Http.expectJson decodeGameView)


previewCommand gameId username command =
    Http.post
        ("http://localhost:8000/game/"
            ++ String.fromInt gameId
            ++ "/player/"
            ++ username
            ++ "/command?preview=true"
        )
        (Http.jsonBody (encodeGameCommand command))
        decodeGameView



-- TODO: Break this up so we don't have to reload the model completely every
-- time.


initWith : Maybe Model -> () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
initWith mModel flags url key =
    let
        parsedRoute =
            parse routes url

        emptyModel =
            { game = Loading
            , gameView = Nothing
            , gameError = Nothing
            , signupError = Nothing
            , playerCommand = ( "", Nothing )
            , route = parsedRoute
            , url = url
            , key = key
            , user = Nothing
            , userLoggedIn = False
            , homePageState = { games = [] }
            }
    in
    case mModel of
        Nothing ->
            -- initial state, no fetching required
            ( emptyModel, Cmd.none )

        Just model ->
            let
                cmds =
                    case parsedRoute of
                        Nothing ->
                            Cmd.none

                        Just (Game gameId) ->
                            case model.user of
                                Nothing ->
                                    Cmd.none

                                Just u ->
                                    Http.send GotGame (getGame u gameId)

                        Just (GamePlayer gameId playerId) ->
                            case model.user of
                                Nothing ->
                                    Cmd.none

                                Just u ->
                                    Http.send GotGame (getGame u gameId)

                        Just Login ->
                            Cmd.none

                        Just (Home username) ->
                            Cmd.none
            in
            ( { model | route = parsedRoute }, cmds )


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init =
    initWith Nothing



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case trace msg of
        Noop ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            initWith (Just model) () url model.key

        --( { model | url = url }, Cmd.none )
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
                    let
                        cmds =
                            List.map
                                (\p -> overlayPlayerMonuments p)
                                (Dict.values gameView.state.players)
                                ++ List.map
                                    (\p -> overlayPlayerCraftsmen p)
                                    (Dict.values gameView.state.players)
                                ++ [ overlayUsedMarkers gameView.state.round.usedMarkers ]
                    in
                    Cmd.batch cmds
            )

        IssueGameCommand command ->
            case model.route of
                Just (GamePlayer gameId username) ->
                    ( model, Http.send GotGame (issueCommand gameId username command) )

                _ ->
                    ( model, Cmd.none )

        PreviewGameCommand command ->
            case model.route of
                Just (GamePlayer gameId username) ->
                    ( model, Http.send GotGame (previewCommand gameId username command) )

                _ ->
                    ( model, Cmd.none )

        UpdateCommand commandString ->
            case Parser.run parseGameCommand commandString of
                Ok cmd ->
                    ( { model | playerCommand = ( commandString, Just cmd ) }, Cmd.none )

                Err _ ->
                    ( { model | playerCommand = ( commandString, Nothing ) }, Cmd.none )

        UpdateUsername username ->
            ( updateUsername username model, Cmd.none )

        UpdatePassword password ->
            ( updatePassword password model, Cmd.none )

        UpdateEmail email ->
            ( updateEmail email model, Cmd.none )

        -- TODO: redirect to home page (list games)
        LoginUser ->
            case model.user of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( model, Http.send GotLogin (postLogin user) )

        SignupUser ->
            case model.user of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( model, Http.send GotSignup (postSignup user) )

        GotSignup result ->
            case result of
                Ok _ ->
                    ( { model | userLoggedIn = True, signupError = Nothing }, Cmd.none )

                Err err ->
                    ( { model | signupError = Just "Conflicting username or email address." }, Cmd.none )

        GotLogin result ->
            case result of
                Ok _ ->
                    case model.user of
                        Nothing ->
                            ( { model | signupError = Just "Something went wrong (user went missing)" }
                            , Cmd.none
                            )

                        Just user ->
                            ( { model | userLoggedIn = True, signupError = Nothing }
                            , Http.send GotUser (getUser user user.username)
                            )

                Err err ->
                    ( { model | signupError = Just "Login failed" }, Cmd.none )

        GetUser ->
            case model.user of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( model, Http.send GotUser (getUser user user.username) )

        GotUser result ->
            case result of
                Ok userView ->
                    let
                        -- TODO: Extract
                        url =
                            { protocol = Url.Http
                            , host = "localhost"
                            , port_ = Just 3000
                            , path = "/home/" ++ userView.username
                            , query = Nothing
                            , fragment = Nothing
                            }

                        cmd =
                            ExtraCmd.send
                                (LinkClicked (Browser.Internal url))
                    in
                    ( updateUsername
                        userView.username
                      <|
                        updateEmail userView.email <|
                            updateId userView.id <|
                                model
                    , cmd
                    )

                Err err ->
                    ( { model | signupError = Just "Could not fetch user" }, Cmd.none )



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

        -- NOTE: This is extremely finnicky. Be careful modifying this, or
        -- the game may completely disappear from the screen.
        gameDiv =
            case model.gameView of
                Just gameView ->
                    renderGame gameView (renderControlPanel model)

                _ ->
                    emptyDiv "Loading..."
    in
    Browser.Document "The Great Zimbabwe" <|
        case model.route of
            Just Login ->
                [ loginPage model ]

            Just (Home username) ->
                case model.user of
                    Nothing ->
                        [ text "Not Found" ]

                    Just user ->
                        [ home user model.homePageState ]

            Nothing ->
                [ text "Not Found" ]

            _ ->
                [ loadingDiv, gameDiv ]


trace val =
    Debug.log (Debug.toString val) val


lookupCurrentPlayer : GameView -> Maybe Player
lookupCurrentPlayer game =
    case game.state.round.currentPlayer of
        Nothing ->
            Nothing

        Just playerId ->
            Dict.get playerId game.state.players


renderGame : GameView -> Html Msg -> Html Msg
renderGame game controlPanel =
    let
        currentPlayerUsername =
            Maybe.map (.info >> .username) (lookupCurrentPlayer game)
    in
    div []
        [ div []
            [ h1 [] [ text game.name ]
            ]
        , gameCanvas
        , if game.state.round.currentPhase == Just GenerosityOfKings then
            renderGenerosityOfKingsState game

          else
            div [] []
        , controlPanel
        , div []
            [ listPlayers game.state.winner currentPlayerUsername (sortedPlayers game)
            ]
        , if game.state.round.currentPhase == Just PreSetup then
            renderPreSetupActionBoard game

          else
            div [] []
        , renderSupply game
        ]


sortedPlayers : GameView -> List Player
sortedPlayers game =
    let
        playerOrder =
            game.state.round.players

        players =
            game.state.players
    in
    catMaybes (List.map (\k -> Dict.get k players) playerOrder)



--game.state.round.playerCommand


renderControlPanel : Model -> Html Msg
renderControlPanel model =
    let
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

        parsedCommand =
            second model.playerCommand

        issueCommandIfPossible cmd =
            case cmd of
                Nothing ->
                    Noop

                Just gameCommand ->
                    IssueGameCommand gameCommand

        previewCommandIfPossible cmd =
            case cmd of
                Nothing ->
                    Noop

                Just gameCommand ->
                    PreviewGameCommand gameCommand
    in
    div []
        [ errorDiv
        , textarea
            [ placeholder "Enter command"
            , value (first model.playerCommand)
            , onInput UpdateCommand
            , style "min-width" "300px"
            , style "min-height" "100px"
            ]
            []
        , div []
            [ span [ style "padding" "10px" ]
                [ button
                    [ onClick (previewCommandIfPossible parsedCommand) ]
                    [ text "Preview" ]
                ]
            , span
                [ style "padding" "10px" ]
                [ button
                    [ onClick (issueCommandIfPossible parsedCommand) ]
                    [ text "Submit" ]
                ]
            ]
        , div [] [ text <| Debug.toString parsedCommand ]
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


renderPlaque plaque =
    case plaque of
        ( PlayerPlaque empire, amount ) ->
            div [] [ text (showEmpire empire ++ " (" ++ String.fromInt amount ++ ")") ]

        ( ShadipinyiPlaque, amount ) ->
            div [] [ text ("Shadipinyi (God of Drunks) (" ++ String.fromInt amount ++ ")") ]


getPlayerById : Int -> GameView -> Maybe Player
getPlayerById playerId gameView =
    Dict.get playerId gameView.state.players



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions =
            \_ ->
                Ports.mapLayoutRendered (\_ -> MapLayoutRendered)
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
