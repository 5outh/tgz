port module Ports exposing (renderMapLayout)

import ApiTypes as ApiTypes exposing (MapLayout)
import Json.Encode as E



-- MapLayout


port renderMapLayout : E.Value -> Cmd msg
