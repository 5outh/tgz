module TheGreatZimbabwe.Tile
  ( allTiles
  , startingTile
  , rotate90
  , Tile(..)
  )
where


import           Data.List              (transpose)
import           TheGreatZimbabwe.Types

newtype Tile = Tile
  { getTile :: [[Square]]
  }

startingTile :: Tile
startingTile = Tile $ map
  fromRow
  [ Row e e s e e s
  , Row e e e e e e
  , Row s e e w e e
  , Row e e c e e s
  , Row e e e e e e
  , Row s e e s e e
  ]

tile1 :: Tile
tile1 = Tile $ map
  fromRow
  [ Row e e e e e o
  , Row e c e e e o
  , Row e e e e e o
  , Row e e e e e o
  , Row e o o d w o
  , Row e o o e e o
  ]

tile2 :: Tile
tile2 = Tile $ map
  fromRow
  [ Row d e e o o o
  , Row e e e o o o
  , Row c e e e i e
  , Row e e e e e e
  , Row e e e e e e
  , Row e e e e e e
  ]

tile3 :: Tile
tile3 = Tile $ map
  fromRow
  [ Row e e o e e e
  , Row e e o c i e
  , Row e e o c e e
  , Row e e o e e e
  , Row e e o o o o
  , Row e e o o o o
  ]

tile4 :: Tile
tile4 = Tile $ map
  fromRow
  [ Row e e o e e e
  , Row e e o c i e
  , Row e e o c e e
  , Row e e o e e e
  , Row e e o o o o
  , Row e e o o o o
  ]

tile5 :: Tile
tile5 = Tile $ map
  fromRow
  [ Row e o e e e e
  , Row e o e i e e
  , Row e o e e e e
  , Row e o i e e e
  , Row e o o o e e
  , Row e o o o e w
  ]

tile6 :: Tile
tile6 = Tile $ map
  fromRow
  [ Row o o o o e e
  , Row o o o o e e
  , Row e e w e e e
  , Row e e e e e e
  , Row d e e e e c
  , Row e e e e e e
  ]

tile7 :: Tile
tile7 = Tile $ map
  fromRow
  [ Row e e e e e e
  , Row e c e e w e
  , Row o o e e e e
  , Row o o e e e e
  , Row o o o o e e
  , Row e o o o e c
  ]

tile8 :: Tile
tile8 = Tile $ map
  fromRow
  [ Row o o o o e w
  , Row o o o o e e
  , Row o o o o e e
  , Row e e i d e e
  , Row e e e e e e
  , Row e e e e e e
  ]

tile9 :: Tile
tile9 = Tile $ map
  fromRow
  [ Row e e o o e e
  , Row e w o o c e
  , Row e e e e e e
  , Row e e e e e e
  , Row e e e e w e
  , Row e e e e e e
  ]

tile10 :: Tile
tile10 = Tile $ map
  fromRow
  [ Row o o e e e e
  , Row o o i e e e
  , Row e e c e e e
  , Row e e e e e e
  , Row e d e e o o
  , Row e e e e o o
  ]

allTiles :: [Tile]
allTiles =
  [tile1, tile2, tile3, tile4, tile5, tile6, tile7, tile8, tile9, tile10]

rotate90 :: Tile -> Tile
rotate90 (Tile squares) = Tile $ reverse (transpose squares)

-- Little helpers

data Row a = Row !a !a !a !a !a !a

fromRow :: Row a -> [a]
fromRow (Row a b c' d' e' f) = [a, b, c', d', e', f]


o, e, s, w, d, i, c :: Square
o = Water
e = Land BlankLand
s = Land StartingArea
w = Land (Resource Wood)
d = Land (Resource Diamonds)
i = Land (Resource Ivory)
c = Land (Resource Clay)
