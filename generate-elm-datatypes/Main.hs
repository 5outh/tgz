{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Data.Proxy
import           Elm.Derive
import           Elm.Module
import           System.IO                      (writeFile)
import qualified TheGreatZimbabwe.Database.Game as DB
import           TheGreatZimbabwe.Types

main :: IO ()
main = do
  writeFile "tgz-frontend/src/ApiTypes.elm.bak" $ makeElmModule
    "ApiTypes"
    [ DefineElm (Proxy :: Proxy PlayerId)
    , DefineElm (Proxy :: Proxy Location)
    , DefineElm (Proxy :: Proxy Username)
    , DefineElm (Proxy :: Proxy PlayerInfo)
    , DefineElm (Proxy :: Proxy Empire)
    , DefineElm (Proxy :: Proxy Resource)
    , DefineElm (Proxy :: Proxy Land)
    , DefineElm (Proxy :: Proxy Square)
    , DefineElm (Proxy :: Proxy MapLayout)
    , DefineElm (Proxy :: Proxy Craftsman)
    , DefineElm (Proxy :: Proxy PrimaryOrSecondary)
    , DefineElm (Proxy :: Proxy TechnologyCard)
    , DefineElm (Proxy :: Proxy Specialist)
    , DefineElm (Proxy :: Proxy God)
    , DefineElm (Proxy :: Proxy Player)
    , DefineElm (Proxy :: Proxy UsedMarker)
    , DefineElm (Proxy :: Proxy GenerosityOfKingsState)
    , DefineElm (Proxy :: Proxy Phase)
    , DefineElm (Proxy :: Proxy Round)
    , DefineElm (Proxy :: Proxy Game)
    -- Database reps
    , DefineElm (Proxy :: Proxy DB.GameView)
    ]
