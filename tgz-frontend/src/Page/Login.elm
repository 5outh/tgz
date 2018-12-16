module Page.Login exposing (loginPage)

import Html
    exposing
        ( Attribute
        , Html
        , a
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
import Html.Attributes
    exposing
        ( for
        , href
        , maxlength
        , minlength
        , name
        , required
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput, onSubmit)
import Model exposing (..)
import Msg exposing (..)


loginPage model =
    div
        []
        [ errorDiv model
        , h3 [] [ text "Login to tgz [alpha]" ]
        , loginForm
        , div [] [ text "or" ]
        , h3 [] [ text "Signup for tgz [alpha]" ]
        , signupForm
        , viewLink "/home/beefcake"
        ]


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


errorDiv model =
    case model.signupError of
        Nothing ->
            div [] []

        Just errorString ->
            div [] [ h2 [] [ text errorString ] ]


loginForm =
    form [ onSubmit LoginUser ]
        [ label [ for "username", minlength 3, maxlength 12 ] [ text "username" ]
        , div [ name "username" ] [ input [ type_ "text", onInput UpdateUsername ] [] ]
        , label [ for "password", minlength 8, maxlength 32 ] [ text "password" ]
        , div [ name "password" ] [ input [ type_ "password", onInput UpdatePassword ] [] ]
        , input [ type_ "submit", value "Login" ] []
        ]


signupForm =
    form [ onSubmit SignupUser ]
        [ label [ for "email", minlength 3, maxlength 32 ] [ text "email" ]
        , div [ name "email" ] [ input [ type_ "text", onInput UpdateEmail ] [] ]
        , label [ for "username", minlength 3, maxlength 12 ] [ text "username" ]
        , div [ name "username" ] [ input [ type_ "text", onInput UpdateUsername ] [] ]
        , label [ for "password", minlength 8, maxlength 32 ] [ text "password" ]
        , div [ name "password" ] [ input [ type_ "password", onInput UpdatePassword ] [] ]
        , input [ type_ "submit", value "Signup" ] []
        ]
