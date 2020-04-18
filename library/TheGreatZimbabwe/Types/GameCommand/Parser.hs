{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
module TheGreatZimbabwe.Types.GameCommand.Parser where

import           Data.Foldable
import           Data.Text                          (Text)
import           Data.Void                          (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer         as L
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
    <*> parseEnd

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
parseAction3 = pure Nothing

parseEnd :: Parser Bool
parseEnd = pure False
