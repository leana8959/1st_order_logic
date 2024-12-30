{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Bifunctor (Bifunctor (first))
import Data.Map.Strict qualified as M
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Text.Megaparsec (errorBundlePretty, runParser)

import Network.Wai.Middleware.Cors (simpleCors)
import Parser (pFormula)
import Solver (solve)
import Types

type PropSolveurAPI =
  ( "api"
      :> ReqBody '[PlainText] String
      :> Post '[PlainText] String
  )

handleSolve :: String -> Handler String
handleSolve input = do
  let parsed =
        first errorBundlePretty $
          runParser pFormula "web" input
  case parsed of
    Left err -> pure err
    Right formula -> pure . showSolutions . solve $ formula
  where
    showSolution :: Valuation -> Int -> String
    showSolution v i =
      unlines $
        ("solution nº" ++ show i) : ((\(var, val) -> var ++ ": " ++ show val) <$> M.toList v)
    showSolutions :: [Valuation] -> String
    showSolutions vs = unlines $ uncurry showSolution <$> zip vs [1 ..]

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
