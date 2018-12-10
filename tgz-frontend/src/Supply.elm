module Supply exposing (godDescription, renderGods, renderSupply, segmentAttrs, showGod)

import ApiTypes
    exposing
        ( Craftsman(..)
        , Empire(..)
        , GameView
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
        , h4
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


renderSupply : GameView -> Html Msg
renderSupply game =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "padding" "3px"
        , style "margin" "3px"
        ]
        [ renderGods game.state.gods
        , renderSpecialists game.state.specialists
        ]


renderGods gods =
    div
        segmentAttrs
    <|
        ([ div [] [ b [] [ text "gods" ] ] ]
            ++ List.map showGod gods
        )


renderSpecialists specialists =
    div
        segmentAttrs
    <|
        ([ div [] [ b [] [ text "specialists" ] ] ]
            ++ List.map showSpecialist specialists
        )


showSpecialist specialist =
    div
        [ style "padding" "3px"
        , style "padding-top" "8px"
        ]
        [ div []
            [ b [] [ text <| ApiTypes.showSpecialist specialist ++ " (" ++ String.fromInt (specialistVR specialist) ++ ")" ]
            ]
        , div []
            [ text (specialistDescription specialist)
            ]
        ]


showGod god =
    div
        [ style "padding" "3px"
        , style "padding-top" "8px"
        ]
        [ div []
            [ b [] [ text <| ApiTypes.showGod god ++ " (" ++ String.fromInt (godVR god) ++ ")" ]
            ]
        , div []
            [ text (godDescription god)
            ]
        ]

specialistDescription specialist = case specialist of
  Shaman ->
      "place resource (2)"

  Nomads ->
      "ignore monument zone (2)"

  RainCeremony ->
      "place water tile (3)"

  Builder ->
      "2 craftsman cost -> card (2)"

  Herd ->
      "[3x] +1 cattle (2)"

godDescription god =
    case god of
        Shadipinyi ->
            "extra plaque"

        Elegua ->
            "3 extra cattle for 1st bid"

        Dziva ->
            "set-price at turn start"

        Eshu ->
            "3 -> 6 range"

        Gu ->
            "1 VR tech cards"

        Obatala ->
            "place 2 monuments"

        Atete ->
            "use resources twice"

        TsuiGoab ->
            "raise using same resources"

        Anansi ->
            "1 cattle ritual goods"

        Qamata _ ->
            "hub payment -> this card"

        Engai ->
            "+2 cattle revenue"

        Xango ->
            "VR -2"



-- TODO reduce duplication


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



-- Victory Requirement for a god


godVR : God -> Int
godVR god =
    case god of
        Anansi ->
            5

        Atete ->
            5

        Dziva ->
            2

        Elegua ->
            4

        Engai ->
            5

        Eshu ->
            4

        Gu ->
            4

        Obatala ->
            7

        Qamata _ ->
            2

        Shadipinyi ->
            4

        TsuiGoab ->
            3

        Xango ->
            -2


specialistVR : Specialist -> Int
specialistVR specialist =
    case specialist of
        Shaman ->
            3

        Nomads ->
            1

        RainCeremony ->
            1

        Builder ->
            2

        Herd ->
            6
