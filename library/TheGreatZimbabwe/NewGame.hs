{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module TheGreatZimbabwe.NewGame where

import           Data.Bifunctor
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Safe                       (headMay)
import           System.Random.Shuffle
import           TheGreatZimbabwe.Error
import qualified TheGreatZimbabwe.MapLayout as MapLayout
import           TheGreatZimbabwe.Text
import           TheGreatZimbabwe.Types

newGame :: [(PlayerId, PlayerInfo)] -> IO (GameEvent 'PreSetup)
newGame playerList = GameEvent <$> do
  -- probably want 'EitherT' here
  eMapLayout <- case (length playerList) of
    2 -> Right <$> MapLayout.twoPlayers
    3 -> Right <$> MapLayout.threePlayers
    4 -> Right <$> MapLayout.fourPlayers
    5 -> Right <$> MapLayout.fivePlayers
    n ->
      pure . internalError $ "Unsupported number of players (" <> tshow n <> ")"
  case eMapLayout of
    Left  err    -> pure (Left err)
    Right layout -> do
      playerOrder <- shuffleM $ map fst playerList
      gameGods    <- S.fromList <$> shuffleM (take 8 allGods)

      let newPlayer playerInfo' = mempty
            { playerInfo               = Just playerInfo'
            , playerVictoryRequirement = mempty { pointsPoints = 20 }
            , playerVictoryPoints      = mempty { pointsPoints = 0 }
            , playerEmpire             = Nothing
            , playerCattle             = 3
            , playerGod                = Nothing
            }
          gamePlayers = M.fromList $ map (second newPlayer) playerList
          gameRound   = Round
            { roundPlayers                = playerOrder
            , roundCurrentPlayer          = headMay playerOrder
            , roundUsedMarkers            = mempty
            , roundGenerosityOfKingsState = GenerosityOfKingsState
              { generosityOfKingsStatePlaques       = [] -- Nothing yet, I don't love this
              , generosityOfKingsStateCattlePool    = 0
              , generosityOfKingsStateLastBid       = Nothing
              , generosityOfKingsStatePlayersPassed = []
              }
            , roundCurrentPhase           = Just PreSetup
            , roundStep                   = 0
            }
          gameSpecialists     = allSpecialists
          gameTechnologyCards = newGameTechnologyCards
          gameWinner          = Nothing
          gameMapLayout       = layout
          gameStep            = 0
          -- TODO: These need to be validated from the box.
          gameWaterTiles      = 6
          gameResourceTiles =
            M.fromList [(Clay, 4), (Wood, 4), (Ivory, 4), (Diamonds, 4)]
          gameCraftsmanTiles = M.fromList $ map (, 3) [minBound ..]
      pure $ Right Game {..}

newGameTechnologyCards :: M.Map Craftsman [TechnologyCard]
newGameTechnologyCards = M.fromList
  [ Potter .: potters
  , IvoryCarver .: ivoryCarvers
  , WoodCarver .: woodCarvers
  , DiamondCutter .: diamondCutters
  , VesselMaker .: vesselMakers
  , ThroneMaker .: throneMakers
  , Sculptor .: sculptors
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
    , TechnologyCard "DiamondCutter2" DiamondCutter 2 3 10
    ]

  vesselMakers =
    [ TechnologyCard "VesselMaker1" VesselMaker 3 2 4
    , TechnologyCard "VesselMaker2" VesselMaker 4 2 4
    ]

  throneMakers =
    [ TechnologyCard "ThroneMaker1" ThroneMaker 3 2 4
    , TechnologyCard "ThroneMaker2" ThroneMaker 4 2 4
    ]

  sculptors =
    [ TechnologyCard "Sculptor1" Sculptor 3 2 4
    , TechnologyCard "Sculptor2" Sculptor 4 2 4
    ]
