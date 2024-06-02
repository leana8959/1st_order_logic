{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Data.Bifunctor (Bifunctor (first))
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
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
  (<**>),
  (<|>),
 )
import System.Console.ANSI (
  Color (Black, Blue, Red),
  ColorIntensity (Dull, Vivid),
  ConsoleIntensity (BoldIntensity, NormalIntensity),
  ConsoleLayer (Foreground),
  SGR (Reset, SetColor, SetConsoleIntensity),
  setSGR,
 )
import System.Exit (exitSuccess)
import System.IO (IOMode (ReadMode), isEOF, openFile)
import Text.Megaparsec (errorBundlePretty, runParser)
import Text.Pretty.Simple (pPrint)

import Parser (pFormula)
import Solver (showSolutions, solve)

accentStyle, decorStyle, errorStyle, resetStyle :: [SGR]
accentStyle = [SetColor Foreground Vivid Blue, SetConsoleIntensity BoldIntensity]
decorStyle = [SetColor Foreground Dull Black, SetConsoleIntensity NormalIntensity]
errorStyle = [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
resetStyle = [Reset]

putWithStyle :: [SGR] -> IO () -> IO ()
putWithStyle st io = setSGR st <* io <* setSGR resetStyle

putWithBorder :: [SGR] -> Text -> Text -> IO ()
putWithBorder st prompt text = do
  let w = ((80 - T.length prompt) `div` 2) - 2
      line = replicate w '='
  putWithStyle decorStyle $ putStr (line ++ " ")
  putWithStyle st $ TIO.putStr prompt
  putWithStyle decorStyle $ putStrLn (" " ++ line)
  TIO.putStrLn text

say, scream :: Text -> Text -> IO ()
say = putWithBorder accentStyle
scream = putWithBorder errorStyle

data Mode = File String | Repl | Stdin deriving (Eq)
instance Show Mode where
  show Repl = "<repl>"
  show Stdin = "<stdin>"
  show (File name) = name

argsParser :: ParserInfo Mode
argsParser =
  info
    (p <**> helper)
    (fullDesc <> header "prop_solveur - a toy logic solver" <> progDesc "Solve logic formulaes")
  where
    p =
      let
        replFlagParser =
          flag'
            Repl
            ( long "repl"
                <> short 'r'
                <> help "Solve interactively"
            )
        fileParser =
          strOption (long "file" <> short 'f' <> help "Read from file, use dash to mean stdin")
            <&> \case
              "-" -> Stdin
              fname -> File fname
       in
        replFlagParser <|> fileParser

main :: IO ()
main = execParser argsParser >>= go

go :: Mode -> IO ()
go m = do
  content >>= output . parse
  when (m == Repl) (go m)
  where
    content = case m of
      File fname -> openFile fname ReadMode >>= TIO.hGetContents
      Stdin -> TIO.getContents
      Repl -> do
        putStrLn "Please enter a logical formula, :q to quit"
        checkEof
        line <- TIO.getLine
        if line == ":q" then exitSuccess else return line
        where
          checkEof = do
            x <- isEOF
            when x (putStrLn "Read EOF... Leaving..." >> exitSuccess)

    parse = first errorBundlePretty . runParser pFormula (show m)

    output = \case
      Right ast -> do
        let sols = solve ast
        say "Parsed" "" <* pPrint ast
        say "Solutions" (T.pack $ showSolutions sols)
        say ("There are " <> (T.pack . show $ length sols) <> " solution(s)") ""
      Left err -> scream "Failed to parse" (T.pack err)
