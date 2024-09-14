{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (Bifunctor (first))
import Data.Functor ((<&>))
import Options.Applicative (
  ParserInfo,
  execParser,
  flag',
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  progDesc,
  short,
  strOption,
  switch,
  (<**>),
  (<|>),
 )
import System.Console.Haskeline
import System.Exit (exitSuccess)
import Text.Megaparsec (errorBundlePretty, runParser)

import Parser (pFormula)
import Solver (showSolutions, solve)

data InputKind = File String | Repl deriving (Eq)
instance Show InputKind where
  show (File fname) = fname
  show Repl = "stdin"

data CliArgs = CliArgs
  { input :: InputKind
  -- ^ where to interact with
  , detail :: Bool
  -- ^ show all solutions or not
  }
  deriving (Eq)

argsParser :: ParserInfo CliArgs
argsParser =
  info
    (CliArgs <$> inputParser <*> detailParser <**> helper)
    (fullDesc <> header "prop_solveur - a toy logic solver" <> progDesc "Solve logic formulaes")
  where
    inputParser =
      let replFlagParser =
            flag' Repl (long "repl" <> short 'r' <> help "Solve interactively")
          fileParser =
            strOption (long "file" <> short 'f' <> help "Read from file, use dash to mean stdin")
              <&> File
       in replFlagParser <|> fileParser

    detailParser =
      switch (long "detail" <> short 'd' <> help "Show all the solutions")

main :: IO ()
main = do
  args <- execParser argsParser
  case args of
    CliArgs {input = File fname} -> runInputTBehavior (useFile fname) defaultSettings (loop args)
    CliArgs {input = Repl} -> runInputT defaultSettings (loop args)

loop :: CliArgs -> InputT IO ()
loop m = do
  content <- loadContent
  output . parse $ content
  loop m
  where
    loadContent = do
      when (input m == Repl) (outputStrLn "Please enter a logical formula")
      mline <- getInputLine "> "
      case mline of
        Nothing -> do
          outputStrLn "Read EOF, exiting"
          liftIO exitSuccess
        Just ":q" -> do
          outputStrLn "Exiting ..."
          liftIO exitSuccess
        Just line -> pure line

    parse = first errorBundlePretty . runParser pFormula (show . input $ m)

    output (Right ast) = do
      let sols = solve ast
      outputStrLn $ "Parsed " <> show ast
      outputStrLn "Solutions"
      outputStrLn $ showSolutions sols
      outputStrLn $ "There are " <> show (length sols) <> " solutions(s)"
    output (Left err) = do
      outputStrLn "Failed to parse ..."
      outputStrLn err
