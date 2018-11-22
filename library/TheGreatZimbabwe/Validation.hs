{-# LANGUAGE OverloadedStrings #-}
module TheGreatZimbabwe.Validation where

import           Prelude                hiding (round)

import           Control.Lens
import           Control.Monad          (when)
import           Data.Monoid
import qualified Data.Text              as T
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.Text
import           TheGreatZimbabwe.Types

phaseIs :: Phase -> Game -> Either GameError ()
phaseIs phase game =
  (currentPhase' /= Just phase)
    `impliesInvalid` (  "You can only perform that action in the "
                     <> tshow phase
                     <> " ( current: "
                     <> tshow currentPhase'
                     <> ")"
                     )
  where currentPhase' = game ^. round . currentPhase

playerIs :: PlayerId -> Game -> Either GameError ()
playerIs playerId game =
  (Just playerId /= current) `impliesInvalid` "It's not your turn."
  where current = game ^. round . currentPlayer

impliesInvalid :: Bool -> T.Text -> Either GameError ()
predicate `impliesInvalid` err = when predicate $ invalidAction err
