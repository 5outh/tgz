module Extra.Maybe exposing (catMaybes, mapMaybe, lastMay)

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

lastMay : Maybe a -> Maybe a -> Maybe a
lastMay m0 m1 = case (m0,m1) of
    (Nothing,Nothing) -> Nothing
    (Just a,Nothing) -> Just a
    (Nothing,Just b) -> Just b
    (Just _,Just b) -> Just b
