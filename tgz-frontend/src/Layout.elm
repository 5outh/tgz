module Layout exposing (coveredLocations, craftsmanDimensions, rotatedDimensions)

import ApiTypes exposing (Craftsman(..), Location, Rotated(..))
import List



-- | Dimensions of placed Craftsmen. Width x Height.


craftsmanDimensions : Craftsman -> ( Int, Int )
craftsmanDimensions craftsman =
    case craftsman of
        Potter ->
            ( 1, 2 )

        IvoryCarver ->
            ( 2, 2 )

        WoodCarver ->
            ( 1, 2 )

        DiamondCutter ->
            ( 2, 2 )

        -- * Secondary
        VesselMaker ->
            ( 2, 2 )

        ThroneMaker ->
            ( 1, 2 )

        Sculptor ->
            ( 2, 2 )


rotatedDimensions : Rotated Craftsman -> ( Int, Int )
rotatedDimensions rotated =
    case rotated of
        UnRotated c ->
            craftsmanDimensions c

        Rotated c ->
            let
                ( a, b ) =
                    craftsmanDimensions c
            in
            ( b, a )



--- TODO: I'm not sure this is necesssary


coveredLocations : Location -> Rotated Craftsman -> List Location
coveredLocations ( x, y ) rCraftsman =
    let
        ( w, h ) =
            rotatedDimensions rCraftsman

        xs =
            case w of
                1 ->
                    [ x ]

                2 ->
                    [ x, x + 1 ]

                _ ->
                    []

        ys =
            case h of
                1 ->
                    [ y ]

                2 ->
                    let
                        mString =
                            String.uncons y
                    in
                    case mString of
                        Just ( c, _ ) ->
                            [ y, String.fromChar <| Char.fromCode (Char.toCode c + 1) ]

                        Nothing ->
                            []

                _ ->
                    []
    in
    List.concatMap (\x0 -> List.map (\y0 -> ( x0, y0 )) ys) xs
