{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TheGreatZimbabwe.NewGame where

import           Data.Bifunctor
import qualified Data.Map.Strict            as M
import           Data.Monoid
import           System.Random.Shuffle
import           TheGreatZimbabwe.Error
import qualified TheGreatZimbabwe.MapLayout as MapLayout
import           TheGreatZimbabwe.Text
import           TheGreatZimbabwe.Types

newGame :: [(PlayerId, PlayerInfo)] -> IO (GameEvent 'Setup)
newGame players = GameEvent <$> do
  -- probably want 'EitherT' here
  eMapLayout <- case (length players) of
    2 -> Right <$> MapLayout.twoPlayers
    3 -> Right <$> MapLayout.threePlayers
    4 -> Right <$> MapLayout.fourPlayers
    5 -> Right <$> MapLayout.fivePlayers
    n ->
      pure . internalError $ "Unsupported number of players (" <> tshow n <> ")"
  case eMapLayout of
    Left  err           -> pure (Left err)
    Right gameMapLayout -> do
      playerOrder <- shuffleM $ map fst players
      let newPlayer playerInfo' = mempty { playerInfo = Alt (Just playerInfo')
                                         , playerVictoryRequirement = Sum 20
                                         , playerVictoryPoints = Sum 0
                                         , playerEmpire = Alt Nothing
                                         , playerCattle = Sum 3
                                         , playerGod = Alt Nothing
                                         }
          gamePlayers = M.fromList $ map (second newPlayer) players
          gameRound   = Round
            { roundPlayers                = playerOrder
            , roundCurrentPlayer          = head playerOrder
            , roundUsedMarkers            = mempty
            , roundGenerosityOfKingsState = GenerosityOfKingsState
              { generosityOfKingsStatePlaques       = [] -- Nothing yet, I don't love this
              , generosityOfKingsStateCattlePool    = 0
              , generosityOfKingsStateLastBid       = 0
              , generosityOfKingsStatePlayersPassed = []
              }
            , roundCurrentPhase           = Setup
            }
          gameCraftsmen = newGameCraftsmen
          gameWinner    = Nothing
      pure $ Right Game {..}

newGameCraftsmen :: M.Map Craftsman [TechnologyCard]
newGameCraftsmen = M.fromList
  [ Potter .: potters
  , IvoryCarver .: ivoryCarvers
  , WoodCarver .: woodCarvers
  , DiamondCutter .: diamondCutters
  ]
 where
  (.:) = (,)
  -- VR/VP/Cost
  potters =
    [ TechnologyCard "Potter1" Potter 3 1 2
    , TechnologyCard "Potter2" Potter 4 1 2
    ]

  ivoryCarvers =
    [ TechnologyCard "IvoryCarver1" IvoryCarver 2 1 2
    , TechnologyCard "IvoryCarver2" IvoryCarver 3 1 2
    ]

  woodCarvers =
    [ TechnologyCard "WoodCarver1" WoodCarver 3 1 2
    , TechnologyCard "WoodCarver2" WoodCarver 4 1 2
    ]

  diamondCutters =
    [ TechnologyCard "DiamondCutter1" DiamondCutter 1 3 10
    , TechnologyCard "DiamondCutter1" DiamondCutter 2 3 10
    ]

  vesselMakers =
    [ TechnologyCard "VesselMaker1" VesselMaker 3 2 4
    , TechnologyCard "VesselMaker2" VesselMaker 4 2 4
    ]
  throneMakers =
    [ TechnologyCard "ThroneMaker1" ThroneMaker 3 2 4
    , TechnologyCard "ThroneMaker1" ThroneMaker 4 2 4
    ]
  sculptors =
    [ TechnologyCard "Sculptor1" Sculptor 3 2 4
    , TechnologyCard "Sculptor1" Sculptor 4 2 4
    ]
