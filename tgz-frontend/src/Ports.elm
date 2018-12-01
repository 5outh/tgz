port module Ports exposing (mapLayoutRendered, overlayPlayerMonuments, renderMapLayout)

import ApiTypes as ApiTypes exposing (MapLayout)
import Json.Encode as E



-- MapLayout


port renderMapLayout : E.Value -> Cmd msg



-- shape: { empire: Empire, monuments: [(Location, Int)] }


port overlayPlayerMonuments : E.Value -> Cmd msg


port mapLayoutRendered : (String -> msg) -> Sub msg
