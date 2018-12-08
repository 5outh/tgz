{-# LANGUAGE OverloadedStrings #-}
module TheGreatZimbabwe.Validation where

import           Prelude                hiding (round)

import           Control.Lens
import           Control.Monad          (when)
import qualified Data.Map.Strict        as M
import           Data.Maybe
import qualified Data.Text              as T
import           GHC.Natural
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.Game
import           TheGreatZimbabwe.Text
import           TheGreatZimbabwe.Types

phaseIs :: Phase -> Game -> Either GameError ()
phaseIs phase game =
  (currentPhase' /= Just phase)
    `impliesInvalid` (  "You can only perform that action in the "
                     <> tshow phase
                     <> " phase. (current: "
                     <> tshow currentPhase'
                     <> ")"
                     )
  where currentPhase' = game ^. round . currentPhase

playerIs :: PlayerId -> Game -> Either GameError ()
playerIs playerId game =
  (Just playerId /= current) `impliesInvalid` "It's not your turn."
  where current = game ^. round . currentPlayer

playerHasCattle :: Natural -> PlayerId -> Game -> Either GameError ()
playerHasCattle amount playerId game = do
  player <- getPlayer playerId game
  (player ^. cattle < fromIntegral amount) `impliesInvalid` notEnoughCattle
 where
  notEnoughCattle =
    "You do not have enough cattle (need " <> tshow amount <> ")."

playerHasSpecialist :: Specialist -> PlayerId -> Game -> Either GameError ()
playerHasSpecialist specialist playerId game = do
  player <- getPlayer playerId game
  (isNothing $ M.lookup specialist (player ^. specialists))
    `impliesInvalid` doesn'tHaveSpecialist
 where
  doesn'tHaveSpecialist =
    "You don't have that Specialist (" <> tshow specialist <> ")."

playerHasGod :: God -> PlayerId -> Game -> Either GameError ()
playerHasGod god' playerId game = do
  player <- getPlayer playerId game
  (player ^. god /= Just god') `impliesInvalid` doesn'tHaveGod
 where
  doesn'tHaveGod =
    "You must adore " <> tshow god' <> " for that action to work."

-- TODO: This probably belongs elsewhere.

runPlayerAction
  :: Phase
  -> PlayerId
  -> Game
  -> (PlayerId -> Game -> PlayerAction actionPhase)
  -> Either GameError Game
runPlayerAction phase playerId game act = do
  phaseIs phase game
  playerIs playerId game
  getPlayerAction (act playerId game)

preSetupAction = runPlayerAction PreSetup
setupAction = runPlayerAction Setup
generosityOfKingsAction = runPlayerAction GenerosityOfKings
religionAndCultureAction = runPlayerAction ReligionAndCulture
