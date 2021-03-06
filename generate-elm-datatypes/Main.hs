{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Data.Proxy
import           Elm.Derive
import           Elm.Module
import           System.IO                           (writeFile)
import qualified TheGreatZimbabwe.Database.Game      as DB
import qualified TheGreatZimbabwe.Database.User      as DB
import           TheGreatZimbabwe.Error
import           TheGreatZimbabwe.ReligionAndCulture
import           TheGreatZimbabwe.Types
import           TheGreatZimbabwe.Types.GameCommand

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
    , DefineElm (Proxy :: Proxy GameCommand)
    , DefineElm (Proxy :: Proxy GameError)
    , DefineElm (Proxy :: Proxy Points)
    , DefineElm (Proxy :: Proxy EmpirePlaque)
    , DefineElm (Proxy :: Proxy ReligionAndCultureCommand1)
    , DefineElm (Proxy :: Proxy UseSpecialist)
    , DefineElm (Proxy :: Proxy ReligionAndCultureCommand3)
    , DefineElm (Proxy :: Proxy RaiseMonumentCommand)
    , DefineElm (Proxy :: Proxy ReligionAndCultureMultiCommand)
    , DefineElm (Proxy :: Proxy SetPrice)
    -- Database reps
    , DefineElm (Proxy :: Proxy DB.GameView)
    , DefineElm (Proxy :: Proxy DB.UserView)
    ]
