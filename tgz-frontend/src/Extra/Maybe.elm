module Extra.Maybe exposing (catMaybes, mapMaybe)

import List exposing (map)


catMaybes : List (Maybe a) -> List a
catMaybes maybes =
    case maybes of
        (Just x) :: rest ->
            x :: catMaybes rest

        Nothing :: rest ->
            catMaybes rest

        [] ->
            []


mapMaybe : (a -> Maybe b) -> List a -> List b
mapMaybe f =
    catMaybes << map f
