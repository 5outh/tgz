{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TheGreatZimbabwe.NewGame where

import           Data.Bifunctor
import qualified Data.Map.Strict            as M
import           Data.Monoid
import qualified Data.Set                   as S
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
          gamePlayers = Merge $ M.fromList $ map (second newPlayer) players
          gameRound   = Round
            { roundPlayers                = playerOrder
            , roundCurrentPlayer          = head playerOrder
            , roundUsedMarkers            = mempty
            , roundGenerosityOfKingsState = GenerosityOfKingsState
              { generosityOfKingsStatePlaques       = [] -- Nothing yet, I don't love this
              , generosityOfKingsStateCattlePool    = Sum 0
              , generosityOfKingsStateLastBid       = Last Nothing
              , generosityOfKingsStatePlayersPassed = []
              }
            , roundCurrentPhase           = Setup
            }
          gameCraftsmen = newGameCraftsmen
          gameWinner    = Alt Nothing
      pure $ Right Game {..}

newGameCraftsmen :: Merge Craftsman (S.Set TechnologyCard)
newGameCraftsmen = Merge $ M.fromList
  [ Potter .: potters
  , IvoryCarver .: ivoryCarvers
  , WoodCarver .: woodCarvers
  , DiamondCutter .: diamondCutters
  ]
 where
  (.:)    = (,)
  -- VR/VP/Cost
  potters = S.fromList
    [ TechnologyCard "Potter1" Potter 3 1 2
    , TechnologyCard "Potter2" Potter 4 1 2
    ]

  ivoryCarvers = S.fromList
    [ TechnologyCard "IvoryCarver1" IvoryCarver 2 1 2
    , TechnologyCard "IvoryCarver2" IvoryCarver 3 1 2
    ]

  woodCarvers = S.fromList
    [ TechnologyCard "WoodCarver1" WoodCarver 3 1 2
    , TechnologyCard "WoodCarver2" WoodCarver 4 1 2
    ]

  diamondCutters = S.fromList
    [ TechnologyCard "DiamondCutter1" DiamondCutter 1 3 10
    , TechnologyCard "DiamondCutter1" DiamondCutter 2 3 10
    ]

  vesselMakers = S.fromList
    [ TechnologyCard "VesselMaker1" VesselMaker 3 2 4
    , TechnologyCard "VesselMaker2" VesselMaker 4 2 4
    ]

  throneMakers = S.fromList
    [ TechnologyCard "ThroneMaker1" ThroneMaker 3 2 4
    , TechnologyCard "ThroneMaker1" ThroneMaker 4 2 4
    ]

  sculptors = S.fromList
    [ TechnologyCard "Sculptor1" Sculptor 3 2 4
    , TechnologyCard "Sculptor1" Sculptor 4 2 4
    ]
