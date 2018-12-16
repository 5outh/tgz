module Page.Home exposing (home, listGames)

import Html
    exposing
        ( Attribute
        , Html
        , button
        , canvas
        , div
        , form
        , h1
        , h2
        , h3
        , img
        , input
        , label
        , li
        , p
        , span
        , text
        , textarea
        , ul
        )
import Html.Attributes exposing (name, required, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Model exposing (..)
import Msg exposing (..)


home user games =
    div
        []
        [ h1 [] [ text <| "Home of " ++ user.username ]
        , listGames games
        ]


listGames games =
    div [] []
