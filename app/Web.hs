{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Bifunctor (Bifunctor (bimap))
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Text.Megaparsec (errorBundlePretty, runParser)

import Parser (pFormula)
import Solver (solve)
import Types (Valuation)

type PropSolveurAPI =
  ( "api"
      :> ReqBody '[JSON, PlainText] String
      :> Post '[JSON] (Either String [Valuation])
  )

handleSolve :: String -> Handler (Either String [Valuation])
handleSolve input = do
  let parsed =
        bimap errorBundlePretty solve $
          runParser pFormula "<web>" input

  pure parsed

server :: Server PropSolveurAPI
server = handleSolve

propSolveurAPI :: Proxy PropSolveurAPI
propSolveurAPI = Proxy

app :: Application
app = simpleCors $ serve propSolveurAPI server

main :: IO ()
main = do
  putStrLn "Started server"
  run 8080 app
