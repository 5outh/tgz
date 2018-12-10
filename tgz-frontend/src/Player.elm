module Player exposing (listPlayers, renderPlayer)

import ApiTypes
    exposing
        ( Craftsman(..)
        , Empire(..)
        , God(..)
        , Player
        , Specialist(..)
        , TechnologyCard
        , TechnologyCardState
        )
import Html
    exposing
        ( Attribute
        , Html
        , b
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
import Html.Attributes exposing (height, id, placeholder, src, style, value, width)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Html.Keyed exposing (node)
import List
import Msg exposing (Msg(..))


listPlayers : Maybe String -> List Player -> Html Msg
listPlayers mCurrentPlayerUsername players =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
    <|
        List.map
            (\player ->
                renderPlayer
                    (Just player.info.username == mCurrentPlayerUsername)
                    player
            )
            players


renderPlayer : Bool -> Player -> Html Msg
renderPlayer isCurrentPlayer player =
    flexItem
        [ style "max-width" "800px"
        , style "display" "flex"
        ]
        [ renderPlayerInfo isCurrentPlayer player
        , renderGod player
        , renderSpecialists player
        , renderTechnologyCards player
        ]


renderTechnologyCards : Player -> Html Msg
renderTechnologyCards player =
    div
        segmentAttrs
        ([ div [ style "text-align" "center" ] [ text "technologies" ] ]
            ++ List.map renderTechnologyCard player.technologyCards
        )


renderTechnologyCard : ( TechnologyCard, TechnologyCardState ) -> Html Msg
renderTechnologyCard ( card, state ) =
    div
        []
        [ text <|
            ApiTypes.showCraftsman card.craftsmanType
                ++ ": "
                ++ String.fromInt state.price
                ++ " ("
                ++ String.fromInt state.cattle
                ++ ")"
        ]


renderGod player =
    div
        segmentAttrs
        [ showGod player.god
        , renderGodDescription player.god
        ]


renderSpecialists : Player -> Html Msg
renderSpecialists player =
    div
        segmentAttrs
    <|
        [ div [ style "text-align" "center" ] [ text "specialists" ]
        ]
            ++ List.map renderSpecialist player.specialists


renderSpecialist ( specialist, cattle ) =
    div
        []
        [ span [] [ text (showSpecialist specialist) ]
        , span [] [ text <| " (" ++ String.fromInt cattle ++ ")" ]
        ]


renderGodDescription mGod =
    case mGod of
        Nothing ->
            div [] []

        Just god ->
            let
                godDescription =
                    case god of
                        Shadipinyi ->
                            "First plaque in Generosity of Kings"

                        Elegua ->
                            "Use up to 3 cattle from common stock on first bid in Generosity of Kings"

                        Dziva ->
                            "Can raise/lower prices at beginning of their turn (separate action)"

                        Eshu ->
                            "Transportation range of 6"

                        Gu ->
                            "Tech cards cost 1 VR"

                        Obatala ->
                            "May place two monuments instead of one"

                        Atete ->
                            "May use resources twice"

                        TsuiGoab ->
                            "Does not need to use different resources to raise monuments"

                        Anansi ->
                            "Only pay 1 cattle per ritual good, regardless of price"

                        Qamata n ->
                            "Hub payment goes to Qamata card instead of common stock (current:" ++ String.fromInt n ++ ")"

                        Engai ->
                            "Receives 2 additional cattle in each revenue phase"

                        Xango ->
                            "VR - 2"
            in
            div
                []
                [ text godDescription ]


showGod god =
    div [ style "text-align" "center" ]
        [ span []
            [ text <|
                "god: "
                    ++ (case god of
                            Nothing ->
                                "—"

                            Just theGod ->
                                ApiTypes.showGod theGod
                       )
            ]
        ]


renderPlayerInfo isCurrentPlayer player =
    div
        segmentAttrs
        [ renderUsername isCurrentPlayer player
        , renderVP player
        , renderVR player
        , renderCattle player
        ]


segmentAttrs =
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "min-height" "100px"
    , style "min-width" "200px"
    , style "max-width" "200px"
    , style "border" "1px solid black"
    , style "margin" "3px"
    , style "padding" "3px"
    ]


renderVP player =
    div
        []
        [ span [] [ text "VP: " ]
        , span [] [ b [] [ text (String.fromInt player.victoryPoints.points) ] ]
        , span
            []
            [ text <| " (" ++ String.fromInt player.victoryPoints.step ++ ")" ]
        , span
            []
            [ text <| " (" ++ String.fromInt (player.victoryRequirement.points - player.victoryPoints.points) ++ " to goal)" ]
        ]


renderVR player =
    div
        []
        [ span [] [ text "VR: " ]
        , span [] [ b [] [ text (String.fromInt player.victoryRequirement.points) ] ]
        , span
            []
            [ text <| " (" ++ String.fromInt player.victoryRequirement.step ++ ")" ]
        ]


renderCattle player =
    div
        []
        [ span [] [ text "🐄: " ]
        , span [] [ b [] [ text (String.fromInt player.cattle) ] ]
        ]


flexItem attrs children =
    div ([ style "padding" "3px" ] ++ attrs) children


renderUsername isCurrentPlayer player =
    let
        playerEmpire =
            case player.empire of
                Nothing ->
                    "—"

                Just empire ->
                    ApiTypes.showEmpire empire

        ( primary, secondary ) =
            empireColors player.empire

        boldIf pred html =
            if pred then
                b [] [ html ]

            else
                html

        info =
            case player.empire of
                Just empire ->
                    " (" ++ ApiTypes.showEmpire empire ++ ")"

                Nothing ->
                    "—"
    in
    div [ style "text-align" "center" ]
        [ span
            [ style "color" primary
            , style "font-size" "30px"

            -- TODO: this is finnicky
            , style "text-shadow" "-1px 0 black, 0 1px black, 1px 0 black, 0 -1px black"
            ]
            [ b [] [ text "■" ] ]
        , span
            [ style "padding-left" "10px" ]
            [ boldIf isCurrentPlayer (text player.info.username)
            ]
        , span [] [ boldIf isCurrentPlayer (text info) ]
        ]


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


showSpecialist : Specialist -> String
showSpecialist specialist =
    case specialist of
        Shaman ->
            "shaman"

        RainCeremony ->
            "rain ceremony"

        Herd ->
            "herd"

        Builder ->
            "builder"

        Nomads ->
            "nomads"
