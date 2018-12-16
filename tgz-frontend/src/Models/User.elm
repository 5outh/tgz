module Models.User exposing (User, encodeUser, updateEmail, updateId, updatePassword, updateUsername)

import Json.Encode as E


type alias User =
    { username : String, password : String, email : String, id : Maybe Int }


updateId id model =
    let
        currentUser =
            model.user

        newUser =
            case model.user of
                Nothing ->
                    { email = "", password = "", username = "", id = Just id }

                Just existingUser ->
                    { existingUser | id = Just id }
    in
    { model | user = Just newUser }


updateEmail email model =
    let
        currentUser =
            model.user

        newUser =
            case model.user of
                Nothing ->
                    { email = email, password = "", username = "", id = Nothing }

                Just existingUser ->
                    { existingUser | email = email }
    in
    { model | user = Just newUser }


updateUsername username model =
    let
        currentUser =
            model.user

        newUser =
            case model.user of
                Nothing ->
                    { username = username, password = "", email = "", id = Nothing }

                Just existingUser ->
                    { existingUser | username = username }
    in
    { model | user = Just newUser }


updatePassword password model =
    let
        currentUser =
            model.user

        newUser =
            case model.user of
                Nothing ->
                    { password = password, username = "", email = "", id = Nothing }

                Just existingUser ->
                    { existingUser | password = password }
    in
    { model | user = Just newUser }


encodeUser : User -> E.Value
encodeUser { email, username, password } =
    E.object <|
        [ ( "username", E.string username )
        , ( "password", E.string password )
        ]
            ++ (if email == "" then
                    []

                else
                    [ ( "email", E.string email ) ]
               )
