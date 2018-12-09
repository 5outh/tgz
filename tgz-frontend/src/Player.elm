module Player exposing (listPlayers, renderPlayer)

import ApiTypes exposing (Empire(..), Player, Specialist(..))
import Html
    exposing
        ( Attribute
        , Html
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
                , if List.isEmpty player.specialists then
                    div [] []

                  else
                    li []
                        [ text
                            ("Specialists: " ++ String.join ", " (List.map showSpecialist player.specialists))
                        ]
                , li [] [ text ("VR: " ++ String.fromInt player.victoryRequirement.points) ]
                , li [] [ text ("VP: " ++ String.fromInt player.victoryPoints.points) ]
                , li [] [ text ("🐄: " ++ String.fromInt player.cattle) ]
                ]
            ]
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


showSpecialist : ( Specialist, Int ) -> String
showSpecialist ( specialist, n ) =
    let
        specialistName =
            case specialist of
                Shaman ->
                    "Shaman"

                RainCeremony ->
                    "Rain Ceremony"

                Herd ->
                    "Herd"

                Builder ->
                    "Builder"

                Nomads ->
                    "Nomads"
    in
    specialistName ++ " (" ++ String.fromInt n ++ ")"
