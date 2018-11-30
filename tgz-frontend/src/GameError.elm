module GameError exposing (GameError(..), decodeGameErrorFromBadStatusResponse)

import Dict
import Http
import Json.Decode exposing (Decoder)
import Json.Helpers exposing (..)


type GameError
    = InvalidAction String
    | InternalError String


decodeGameErrorFromBadStatusResponse : Http.Error -> Maybe GameError
decodeGameErrorFromBadStatusResponse response =
    case response of
        Http.BadStatus { body } ->
            case Json.Decode.decodeString decodeGameError body of
                Ok gameError ->
                    Just gameError

                Err err ->
                    Nothing

        _ ->
            Nothing


decodeGameError : Decoder GameError
decodeGameError =
    let
        jsonDecDictGameError =
            Dict.fromList
                [ ( "InvalidAction"
                  , Json.Decode.lazy
                        (\_ -> Json.Decode.map InvalidAction Json.Decode.string)
                  )
                , ( "InternalError"
                  , Json.Decode.lazy
                        (\_ -> Json.Decode.map InternalError Json.Decode.string)
                  )
                ]
    in
    decodeSumObjectWithSingleField "GameError" jsonDecDictGameError
