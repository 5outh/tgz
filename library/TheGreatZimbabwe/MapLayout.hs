{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module TheGreatZimbabwe.MapLayout where

import           Control.Monad.Random   (uniform)
import           Data.Foldable
import qualified Data.Map.Strict        as M
import           Data.Maybe
import           System.Random.Shuffle
import           TheGreatZimbabwe.Tile
import           TheGreatZimbabwe.Types

-- Note: I think this is easier if we eat the randomness cost at the beginning
-- and not have to worry about threading a StdGen througout the game.

data Tiles4 = Tiles4
  { tiles4Tile1 :: Tile
  , tiles4Tile2 :: Tile
  , tiles4Tile3 :: Tile
  , tiles4Tile4 :: Tile
  }

data Tiles9 = Tiles9
  { tiles9Tile1 :: Maybe Tile
  , tiles9Tile2 :: Maybe Tile
  , tiles9Tile3 :: Maybe Tile
  , tiles9Tile4 :: Maybe Tile
  , tiles9Tile5 :: Maybe Tile
  , tiles9Tile6 :: Maybe Tile
  , tiles9Tile7 :: Maybe Tile
  , tiles9Tile8 :: Maybe Tile
  , tiles9Tile9 :: Maybe Tile
  }
q1Locations :: [Location]
q1Locations = flip Location <$> ['a' .. 'f'] <*> [1 .. 6]

q2Locations :: [Location]
q2Locations = flip Location <$> ['a' .. 'f'] <*> [7 .. 12]

q3Locations :: [Location]
q3Locations = flip Location <$> ['a' .. 'f'] <*> [13 .. 18]

q4Locations :: [Location]
q4Locations = flip Location <$> ['g' .. 'l'] <*> [1 .. 6]

q5Locations :: [Location]
q5Locations = flip Location <$> ['g' .. 'l'] <*> [7 .. 12]

q6Locations :: [Location]
q6Locations = flip Location <$> ['g' .. 'l'] <*> [13 .. 18]

q7Locations :: [Location]
q7Locations = flip Location <$> ['m' .. 'r'] <*> [1 .. 6]

q8Locations :: [Location]
q8Locations = flip Location <$> ['m' .. 'r'] <*> [7 .. 12]

q9Locations :: [Location]
q9Locations = flip Location <$> ['m' .. 'r'] <*> [13 .. 18]

fromTiles9 :: Tiles9 -> MapLayout
fromTiles9 Tiles9 {..} = MapLayout $ M.fromList $ concat
  [ tileLayout q1Locations tiles9Tile1
  , tileLayout q2Locations tiles9Tile2
  , tileLayout q3Locations tiles9Tile3
  , tileLayout q4Locations tiles9Tile4
  , tileLayout q5Locations tiles9Tile5
  , tileLayout q6Locations tiles9Tile6
  , tileLayout q7Locations tiles9Tile7
  , tileLayout q8Locations tiles9Tile8
  , tileLayout q9Locations tiles9Tile9
  ]
 where
  tileLayout :: [Location] -> Maybe Tile -> [(Location, Square)]
  tileLayout locations mTile = catMaybes $ case mTile of
    Nothing   -> []
    Just tile -> map Just $ zip locations (concat $ getTile tile)

fromTiles4 :: Tiles4 -> MapLayout
fromTiles4 Tiles4 {..} =
  MapLayout
    $  M.fromList
    $  zip q1Locations (concat (getTile tiles4Tile1))
    ++ zip q2Locations (concat (getTile tiles4Tile2))
    ++ zip q4Locations (concat (getTile tiles4Tile3))
    ++ zip q5Locations (concat (getTile tiles4Tile4))

printMapLayout :: MapLayout -> IO ()
printMapLayout (MapLayout layout) = do
  let xs = [1 .. 18]
      ys = ['a' .. 'r']

  for_ ys $ \y' -> do
    let str = flip concatMap xs $ \x' ->
          let loc = "" -- y : show x <> ": "
              chr = case M.lookup (Location x' y') layout of
                Nothing -> ' '
                Just c  -> case c of
                  Water     -> 'O'
                  Land land -> case land of
                    StartingArea      -> 'S'
                    BlankLand         -> 'E'
                    Resource resource -> case resource of
                      Clay     -> 'C'
                      Wood     -> 'W'
                      Ivory    -> 'I'
                      Diamonds -> 'D'
          in  loc ++ [chr] ++ " "

    putStrLn str

twoPlayers :: IO MapLayout
twoPlayers = do
  rotatedTiles          <- traverse rotateSomeAmount allTiles
  rotatedStart          <- rotateSomeAmount startingTile
  [tileA, tileB, tileC] <- take 3 <$> shuffleM rotatedTiles
  pure $ fromTiles4 $ Tiles4 rotatedStart tileA tileB tileC

threePlayers :: IO MapLayout
threePlayers = do
  rotatedTiles    <- traverse rotateSomeAmount allTiles
  rotatedStart    <- rotateSomeAmount startingTile
  [a, b, c, d, e] <- take 5 <$> shuffleM rotatedTiles
  pure . fromTiles9 $ Tiles9 Nothing
                             Nothing
                             (Just a)
                             Nothing
                             (Just rotatedStart)
                             (Just b)
                             (Just c)
                             (Just d)
                             (Just e)

fourPlayers :: IO MapLayout
fourPlayers = do
  rotatedTiles       <- traverse rotateSomeAmount allTiles
  rotatedStart       <- rotateSomeAmount startingTile
  [a, b, c, d, e, f] <- take 6 <$> shuffleM rotatedTiles
  pure . fromTiles9 $ Tiles9 (Just a)
                             (Just b)
                             Nothing
                             (Just c)
                             (Just rotatedStart)
                             (Just d)
                             Nothing
                             (Just e)
                             (Just f)


fivePlayers :: IO MapLayout
fivePlayers = do
  rotatedTiles             <- traverse rotateSomeAmount allTiles
  rotatedStart             <- rotateSomeAmount startingTile
  [a, b, c, d, e, f, g, h] <- take 8 <$> shuffleM rotatedTiles
  pure . fromTiles9 $ Tiles9 (Just a)
                             (Just b)
                             (Just c)
                             (Just d)
                             (Just rotatedStart)
                             (Just e)
                             (Just f)
                             (Just g)
                             (Just h)

rotateSomeAmount :: Tile -> IO Tile
rotateSomeAmount tile = do
  rotate <- uniform
    [id, rotate90, rotate90 . rotate90, rotate90 . rotate90 . rotate90]
  pure $ rotate tile
