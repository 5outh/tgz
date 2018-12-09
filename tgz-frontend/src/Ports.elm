port module Ports exposing
    ( mapLayoutRendered
    , overlayPlayerCraftsmen
    , overlayPlayerMonuments
    , overlayUsedMarkers
    , playerMonumentsOverlaid
    , renderMapLayout
    )

import ApiTypes as ApiTypes exposing (MapLayout)
import Json.Encode as E



-- outgoing ports


port renderMapLayout : E.Value -> Cmd msg


port overlayPlayerMonuments : E.Value -> Cmd msg


port overlayPlayerCraftsmen : E.Value -> Cmd msg


port overlayUsedMarkers : E.Value -> Cmd msg



-- incoming ports ports


port mapLayoutRendered : (String -> msg) -> Sub msg


port playerMonumentsOverlaid : (String -> msg) -> Sub msg
