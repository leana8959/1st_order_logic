{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (Bifunctor (first))
import Data.Functor ((<&>))
import Data.Map.Strict qualified as M
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
import System.Console.ANSI (
  Color (Blue, Red),
  ColorIntensity (Vivid),
  ConsoleLayer (Foreground),
  SGR (SetColor),
  setSGRCode,
 )
import System.Console.Haskeline
import System.Exit (exitSuccess)
import Text.Megaparsec (errorBundlePretty, runParser)

import Parser (pFormula)
import Solver (freeVars, solve, valuations)

import Types

data InputKind = File String | Repl deriving (Eq)
instance Show InputKind where
  show (File fname) = fname
  show Repl = "<stdin>"

data CliArgs = CliArgs
  { input :: InputKind
  -- ^ where to interact with
  , detail :: Bool
  -- ^ show all solutions or not
  }
  deriving (Eq)

withColor :: [SGR] -> String -> String
withColor codes s = set <> s <> unset
  where
    set = setSGRCode codes
    unset = setSGRCode []

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

data ReplInputKind = Content String | NewState CliArgs

loop :: CliArgs -> InputT IO ()
loop m = do
  got <- loadContent
  case got of
    Content content -> do
      output . parse $ content
      loop m
    NewState m' -> loop m'
  where
    loadContent :: InputT IO ReplInputKind
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
        Just ":set detail" -> do
          outputStrLn "Enabled detail. Will print all solutions"
          pure (NewState (m {detail = True}))
        Just ":set nodetail" -> do
          outputStrLn "Disabled detail. Will not print all solutions"
          pure (NewState (m {detail = False}))

        Just line -> pure (Content line)

    parse = first errorBundlePretty . runParser pFormula (show . input $ m)

    output (Right ast) = do
      let sols = solve ast
      let stats = showStats ast
      outputStrLn $ "Parsed " <> withColor [SetColor Foreground Vivid Blue] (show ast)
      outputStrLn "Solutions"
      outputStrLn stats
      when (detail m) (outputStrLn $ showSolutions sols)
      outputStrLn
        . withColor [SetColor Foreground Vivid Blue]
        $ "There are " <> show (length sols) <> " solutions(s)"
      where
        showSolution :: Valuation -> Int -> String
        showSolution v i =
          unlines $
            ("solution nº" ++ show i) : ((\(var, val) -> var ++ ": " ++ show val) <$> M.toList v)

        showSolutions :: [Valuation] -> String
        showSolutions vs = unlines $ uncurry showSolution <$> zip vs [1 ..]

        showStats :: Formula -> String
        showStats f =
          unlines $
            ["This is a ✨tautology✨" | valuationsCount == trues]
              ++ ["This is a 🚫contradiction🚫" | trues == 0]
              ++ [ "There are "
                    <> withColor [SetColor Foreground Vivid Blue] (show propsCount)
                    <> " propositional variables,"
                 , "which is "
                    <> withColor [SetColor Foreground Vivid Blue] (show valuationsCount)
                    <> " possibilties,"
                 , "where among them "
                    <> withColor [SetColor Foreground Vivid Blue] (show trues)
                    <> " are valuated as true"
                 ]
          where
            propsCount = length (freeVars f)
            valuationsCount = length $ valuations (freeVars f)
            trues = length $ solve f
    output (Left err) = do
      outputStrLn "Failed to parse ..."
      outputStrLn
        . withColor [SetColor Foreground Vivid Red]
        $ err
