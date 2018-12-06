{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
module TheGreatZimbabwe.MapLayout where

import           Control.Lens
import           Control.Monad.Random   (uniform)
import           Data.Foldable
import qualified Data.List              as L
import qualified Data.Map.Strict        as M
import           Data.Maybe
import qualified Data.Set               as S
import           GHC.Natural            (Natural)
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

mkLocation :: Char -> Natural -> Location
mkLocation c n = Location n c

q1Locations :: [Location]
q1Locations = mkLocation <$> ['a' .. 'f'] <*> [1 .. 6]

q2Locations :: [Location]
q2Locations = mkLocation <$> ['a' .. 'f'] <*> [7 .. 12]

q3Locations :: [Location]
q3Locations = mkLocation <$> ['a' .. 'f'] <*> [13 .. 18]

q4Locations :: [Location]
q4Locations = mkLocation <$> ['g' .. 'l'] <*> [1 .. 6]

q5Locations :: [Location]
q5Locations = mkLocation <$> ['g' .. 'l'] <*> [7 .. 12]

q6Locations :: [Location]
q6Locations = mkLocation <$> ['g' .. 'l'] <*> [13 .. 18]

q7Locations :: [Location]
q7Locations = mkLocation <$> ['m' .. 'r'] <*> [1 .. 6]

q8Locations :: [Location]
q8Locations = mkLocation <$> ['m' .. 'r'] <*> [7 .. 12]

q9Locations :: [Location]
q9Locations = mkLocation <$> ['m' .. 'r'] <*> [13 .. 18]

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

locationNeighbors4 :: Location -> [Location]
locationNeighbors4 location =
  [ locationLeft location
  , locationRight location
  , locationAbove location
  , locationBelow location
  ]

locationNeighbors8 :: Location -> [Location]
locationNeighbors8 location =
  [ locationLeft location
  , locationRight location
  , locationAbove location
  , locationBelow location
  , locationLeft (locationAbove location)
  , locationRight (locationAbove location)
  , locationLeft (locationBelow location)
  , locationRight (locationBelow location)
  ]

fillWater :: Location -> MapLayout -> S.Set Location
fillWater loc (MapLayout layout) = go S.empty (S.singleton loc) [loc]
 where
  go visited waters (loc : stack) = case M.lookup loc layout of
    Just Water ->
      let immediateNeighbors = locationNeighbors4 loc L.\\ (S.toList visited)
          neighborSquares    = mapMaybe
            (\loc -> (loc, ) <$> M.lookup loc layout)
            immediateNeighbors
          neighborWaters = filter (\(_, s) -> s == Water) neighborSquares
      in  go (S.insert loc visited)
             (S.fromList (map fst neighborWaters) `S.union` waters)
             (map fst neighborWaters ++ stack)
    Nothing -> go visited waters stack
  go _ waters [] = waters

data MapNode = LandNode Location | WaterNode Int -- (S.Set Location)
  deriving (Show, Eq, Ord)

data MapGraph = MapGraph
  { mapGraphNodes      :: M.Map Location MapNode
  , mapGraphWaterNodes :: M.Map Int (S.Set Location)
  , mapGraphEdges      :: M.Map MapNode (S.Set MapNode)
  } deriving (Show, Eq)

mapConnectionsN :: Int -> MapGraph -> S.Set Location -> S.Set Location
mapConnectionsN 0 graph locations = locations
mapConnectionsN n graph locations =
  let connections = mapConnectionsMany graph locations
  in  connections <> mapConnectionsN (n - 1) graph connections

mapConnectionsMany :: MapGraph -> S.Set Location -> S.Set Location
mapConnectionsMany graph locations =
  S.unions $ S.toList $ S.map (mapConnections graph) locations

mapConnections :: MapGraph -> Location -> S.Set Location
mapConnections MapGraph {..} location = case M.lookup location mapGraphNodes of
  Just node ->
    let mEdges      = M.lookup node mapGraphEdges
        toLocations = \case
          WaterNode key       -> M.lookup key mapGraphWaterNodes
          LandNode  location1 -> Just (S.singleton location1)
    in  case mEdges of
          Nothing    -> S.empty
          Just edges -> mconcat $ mapMaybe toLocations (S.toList edges)
  Nothing -> S.empty

constructMapGraph :: MapLayout -> MapGraph
constructMapGraph mapLayout@(MapLayout layout) =
  let nodes = buildNodes 0 empty (M.keys layout) in connectEdges nodes
 where
  empty = MapGraph mempty mempty mempty
  -- this gets the *nodes* embedded in the graph.
  buildNodes _        graph []           = graph
  buildNodes waterKey graph (key : keys) = case (M.lookup key layout) of
    Nothing     -> error "failed lookup cannot happen"
    Just square -> case square of
      Land _ ->
        let newGraph = graph
              { mapGraphNodes = M.insert key
                                         (LandNode key)
                                         (mapGraphNodes graph)
              }
        in  buildNodes waterKey newGraph keys

      Water
        -> let
             existingWaterLocations =
               mconcat $ M.elems (mapGraphWaterNodes graph)

             waterBody = fillWater key mapLayout
             -- Mappings from each location in the water to the water node.
             locationMappings =
               M.fromList $ map (, WaterNode waterKey) $ S.toList waterBody
             newGraph = graph
               { mapGraphNodes = locationMappings `M.union` mapGraphNodes graph
               , mapGraphWaterNodes = M.insert waterKey
                                               waterBody
                                               (mapGraphWaterNodes graph)
               }
           in
             -- if we've already built a water body containing this location,
             -- skip building a new one
             if key `S.notMember` existingWaterLocations
               then buildNodes (succ waterKey) newGraph keys
               else buildNodes waterKey graph keys

  -- for each graph node, look up neighbors.
  -- if neighbhor is land, add edge from node to land.
  connectEdges graph = graph { mapGraphEdges = graphEdges }
   where
    graphEdges =
      M.fromListWith (<>) $ S.toList $ S.map (_2 %~ S.singleton) $ go
        S.empty
        (M.keys (mapGraphNodes graph))
    go edges []           = edges
    go edges (key : keys) = case M.lookup key (mapGraphNodes graph) of
      Nothing   -> error "failed lookup cannot happen"
      Just node -> case node of
        WaterNode waterKey -> go edges keys -- edges are added in the 'LandNode' stage.
        LandNode location ->
          let neighbors   = locationNeighbors8 location
              connections = mapMaybe
                (\loc -> M.lookup loc (mapGraphNodes graph))
                neighbors
              -- This should connect:
              -- land -> land
              -- land -> water
              -- water -> land
              newEdges = S.fromList (map (LandNode location, ) connections)
                <> S.fromList (map (, LandNode location) connections)
          in  go (edges <> newEdges) keys
