{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
module TheGreatZimbabwe.Types.GameCommand.Parser where

import           Data.Either                         (partitionEithers)
import           Data.Foldable
import           Data.List.NonEmpty                  (NonEmpty (..))
import qualified Data.List.NonEmpty                  as NE
import           Data.Maybe                          (fromMaybe)
import           Data.Text                           (Text)
import           Data.Void                           (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer          as L
import           TheGreatZimbabwe.ReligionAndCulture
import           TheGreatZimbabwe.Types
import           TheGreatZimbabwe.Types.GameCommand

type Parser = Parsec Void Text

parseGameCommand :: Parser GameCommand
parseGameCommand =
  parsePass <|> parseBid <|> parseChooseEmpire <|> parsePlaceStartingMonument

spaceConsumer :: Parser ()
spaceConsumer =
  L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol' spaceConsumer

parsePass :: Parser GameCommand
parsePass = Pass <$ lexeme (symbol "pass")

parseBid :: Parser GameCommand
parseBid = Bid <$> do
  lexeme (symbol "bid")
  lexeme L.decimal

parseChooseEmpire :: Parser GameCommand
parseChooseEmpire = ChooseEmpire <$> do
  lexeme (string "choose-empire")
  asum
    [ Kilwa <$ symbol "kilwa"
    , Mutapa <$ symbol "mutapa"
    , Zulu <$ symbol "zulu"
    , Lozi <$ symbol "lozi"
    , Mapungubwe <$ symbol "mapungubwe"
    ]

parseLocation :: Parser Location
parseLocation = flip Location <$> oneOf ['a' .. 'z'] <*> lexeme L.decimal

parsePlaceStartingMonument :: Parser GameCommand
parsePlaceStartingMonument =
  PlaceStartingMonument <$> (symbol "place-starting-monument" *> parseLocation)

parseReligionAndCultureCommand :: Parser GameCommand
parseReligionAndCultureCommand =
  ReligionAndCultureCommand <$> parseReligionAndCultureMultiCommand

parseReligionAndCultureMultiCommand :: Parser ReligionAndCultureMultiCommand
parseReligionAndCultureMultiCommand = do
  ReligionAndCultureMultiCommand
    <$> parseDziva
    <*> parseAction1
    <*> parseAction2
    <*> parseAction3

parseDziva :: Parser (Maybe [SetPrice])
parseDziva = label "Dziva Command" $ optional (many parseSetPrice)

parseSetPrice :: Parser SetPrice
parseSetPrice = label "SetPrice" $ do
  symbol "set-price"
  craftsman <- parseCraftsman
  price     <- lexeme L.decimal
  pure $ SetPrice price craftsman

parseCraftsman :: Parser Craftsman
parseCraftsman = label "Craftsman" $ asum
  [ Potter <$ symbol "potter"
  , IvoryCarver <$ symbol "ivory-carver"
  , WoodCarver <$ symbol "wood-carver"
  , DiamondCutter <$ symbol "diamond-cutter"
  , VesselMaker <$ symbol "vessel-maker"
  , ThroneMaker <$ symbol "throne-maker"
  , Sculptor <$ symbol "sculptor"
  ]

parseAction1 :: Parser (Maybe ReligionAndCultureCommand1)
parseAction1 = optional $ parseChooseGod <|> parseChooseSpecialist

parseChooseGod :: Parser ReligionAndCultureCommand1
parseChooseGod = ChooseGod <$> do
  symbol "choose-god"
  asum
    [ Shadipinyi <$ symbol "shadipinyi"
    , Elegua <$ symbol "elegua"
    , Dziva <$ symbol "dziva"
    , Eshu <$ symbol "eshu"
    , Gu <$ symbol "gu"
    , Obatala <$ symbol "obatala"
    , Atete <$ symbol "atete"
    , TsuiGoab <$ symbol "tsui-goab"
    , Anansi <$ symbol "anansi"
    , Qamata 0 <$ symbol "qamata"
    , Engai <$ symbol "engai"
    , Xango <$ symbol "xango"
    ]

parseChooseSpecialist :: Parser ReligionAndCultureCommand1
parseChooseSpecialist = ChooseSpecialist <$> do
  symbol "choose-specialist"
  asum
    [ Shaman <$ symbol "shaman"
    , RainCeremony <$ symbol "rain-ceremony"
    , Herd <$ symbol "herd"
    , Builder <$ symbol "builder"
    , Nomads <$ symbol "nomads"
    ]

parseAction2 :: Parser (Maybe UseSpecialist)
parseAction2 = optional $ do
  asum
    [ parseUseShaman
    , parseUseRainCeremony
    , parseUseHerd
    , parseUseBuilder
    , parseUseNomads
    ]

parseUseShaman :: Parser UseSpecialist
parseUseShaman = do
  symbol "shaman"
  UseShaman <$> parseResource <*> parseLocation

parseResource :: Parser Resource
parseResource = asum
  [ Clay <$ symbol "clay"
  , Wood <$ symbol "wood"
  , Ivory <$ symbol "ivory"
  , Diamonds <$ symbol "diamonds"
  ]

parseUseRainCeremony :: Parser UseSpecialist
parseUseRainCeremony = do
  symbol "rain-ceremony"
  UseRainCeremony <$> parseLocation <*> parseLocation

parseUseHerd :: Parser UseSpecialist
parseUseHerd = do
  symbol "herd"
  UseHerd <$> lexeme L.decimal

parseUseBuilder :: Parser UseSpecialist
parseUseBuilder = UseBuilder <$ symbol "builder"

parseUseNomads :: Parser UseSpecialist
parseUseNomads = UseNomads <$ symbol "nomads"

parseAction3 :: Parser (Maybe ReligionAndCultureCommand3)
parseAction3 =
  optional $ parseBuildMonuments <|> parsePlaceCraftsmen <|> parseRaiseMonuments

parseBuildMonuments :: Parser ReligionAndCultureCommand3
parseBuildMonuments =
  BuildMonuments <$> (many1 $ symbol "place-monument" *> parseLocation)

parsePlaceCraftsmen :: Parser ReligionAndCultureCommand3
parsePlaceCraftsmen = do
  eActions <- many1 (eitherP parsePlaceCraftsman parseSetPrice)
  let (placeCraftsmen, setPrices) = partitionEithers $ NE.toList eActions
  pure $ PlaceCraftsmen placeCraftsmen setPrices
 where
  parseRotatedCraftsman = do
    string "rotated"
    craftsman <- between (symbol "(") (symbol ")") parseCraftsman
    pure $ Rotated craftsman
  parsePlaceCraftsman = do
    symbol "place-craftsman"
    craftsman <- parseRotatedCraftsman <|> (UnRotated <$> parseCraftsman)
    location  <- parseLocation
    pure (location, craftsman)

parseRaiseMonuments :: Parser ReligionAndCultureCommand3
parseRaiseMonuments = fmap (RaiseMonuments . NE.toList) $ many1 $ do
  symbol "raise-monument"
  loc      <- parseLocation
  commands <- many parseRaiseMonumentCommand
  pure $ (loc, commands)
 where
  parseUseHub       = UseHub <$> (symbol "use-hub" *> parseLocation)
  parseUseCraftsman = do
    symbol "use-craftsman"
    UseCraftsman <$> parseLocation <*> parseLocation
  parseRaiseMonumentCommand = parseUseHub <|> parseUseCraftsman

many1 :: Parser a -> Parser (NonEmpty a)
many1 p = do
  a    <- p
  rest <- optional $ many p
  pure $ a :| fromMaybe [] rest
