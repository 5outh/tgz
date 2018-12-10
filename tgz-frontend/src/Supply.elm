module Supply exposing (renderSupply)

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
        , hr
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
import Layout
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
        [ renderListPanelWith showGod "gods" game.state.gods
        , renderListPanelWith showSpecialist "specialists" game.state.specialists
        , renderListPanelWith showTechnologyCards "technologies" game.state.technologyCards
        , renderGeneralSupply game.state
        ]


showTechnologyCards ( craftsman, cards ) =
    div []
        ([ div [ style "padding-top" "5px" ] [ b [] [ text (ApiTypes.showCraftsman craftsman) ] ] ]
            ++ List.map showTechnologyCard cards
        )


showTechnologyCard { name, craftsmanType, victoryRequirement, victoryPoints, cost } =
    div
        []
        [ span [] [ text <| "VR: " ++ String.fromInt victoryRequirement ]
        , span [] [ text <| ", VP: " ++ String.fromInt victoryPoints ]
        , span [] [ text <| ", cost: " ++ String.fromInt cost ]
        ]


renderGeneralSupply { resourceTiles, waterTiles, craftsmanTiles } =
    div
        segmentAttrs
        [ div [ style "text-align" "center" ] [ b [] [ text "supply" ] ]
        , hr [ width 100 ] []
        , showCraftsmanTiles craftsmanTiles
        , showResourceTiles resourceTiles waterTiles
        ]


showResourceTiles resources waterCount =
    div
        []
        [ div
            [ style "margin-top" "8px"
            , style "margin-bottom" "8px"
            ]
            [ b [] [ text "resources" ] ]
        , div
            [ style "display" "flex" ]
            [ span [ style "width" "70%" ] [ text "water" ]
            , span [ style "width" "20%" ] [ text "1x2" ]
            , span [ style "width" "10%" ] [ text <| "x" ++ String.fromInt waterCount ]
            ]
        , div [] (List.map showResourceTile resources)
        ]


showResourceTile ( resource, n ) =
    div [ style "display" "flex" ]
        [ span [ style "width" "70%" ] [ text (ApiTypes.showResource resource) ]
        , span [ style "width" "20%" ] [ text "1x1" ]
        , span [ style "width" "10%" ] [ text <| "x" ++ String.fromInt n ]
        ]


showCraftsmanTiles craftsmanTiles =
    div
        []
        [ div
            [ style "margin-top" "8px"
            , style "margin-bottom" "8px"
            ]
            [ b [] [ text "craftsmen" ] ]
        , div [] (List.map showCraftsmanTile craftsmanTiles)
        ]


showCraftsmanTile ( craftsman, n ) =
    div
        [ style "display" "flex"
        ]
        [ span [ style "width" "70%" ] [ text (ApiTypes.showCraftsman craftsman) ]
        , span [ style "width" "20%" ] [ text (showCraftsmanDimensions craftsman) ]
        , span [ style "width" "10%" ] [ text <| "x" ++ String.fromInt n ]
        ]


showCraftsmanDimensions craftsman =
    let
        ( w, h ) =
            Layout.craftsmanDimensions craftsman
    in
    String.fromInt w ++ "x" ++ String.fromInt h


renderListPanelWith show name things =
    div
        segmentAttrs
        [ div [ style "text-align" "center" ] [ b [] [ text name ] ]
        , hr [ width 100 ] []
        , div [] (List.map show things)
        ]


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
            [ b [] [ text <| ApiTypes.showGod god ++ " (" ++ String.fromInt (godVR god) ++ " VR)" ]
            ]
        , div []
            [ text (godDescription god)
            ]
        ]


specialistDescription specialist =
    case specialist of
        Shaman ->
            "place resource (2 VR)"

        Nomads ->
            "ignore monument zone (2 VR)"

        RainCeremony ->
            "place water tile (3 VR)"

        Builder ->
            "2 craftsman cost -> card (2 VR)"

        Herd ->
            "+1 cattle (x1-3) (2 VR)"


godDescription god =
    case god of
        Shadipinyi ->
            "extra plaque 🍺"

        Elegua ->
            "3 extra cattle for 1st bid"

        Dziva ->
            "set-price at turn start"

        Eshu ->
            "+3 range"

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
