module Page.Login exposing (loginPage)

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
import Html.Attributes exposing (for, maxlength, minlength, name, required, type_)
import Html.Events exposing (onClick, onInput)
import Model exposing (..)
import Msg exposing (..)


loginPage =
    div
        []
        [ h3 [] [ text "Login to tgz [alpha]" ]
        , loginForm
        ]


loginForm =
    form []
        [ label [ for "username", minlength 3, maxlength 12 ] [ text "username" ]
        , div [ name "username" ] [ input [ type_ "text", onInput UpdateUsername ] [] ]
        , label [ for "password", minlength 8, maxlength 32 ] [ text "password" ]
        , div [ name "password" ] [ input [ type_ "password", onInput UpdatePassword ] [] ]
        , button [ onClick LoginUser ] [ text "login" ]
        ]
