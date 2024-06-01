{-# LANGUAGE OverloadedStrings #-}

module Parser (pFormula) where

import Data.Char (isAlpha)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (
  MonadParsec (eof, takeWhile1P, try),
  Parsec,
  choice,
  chunk,
  many,
  skipMany,
  some,
  (<|>),
 )
import Text.Megaparsec.Char (eol, hspace1)

import Types (Formula (..))

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = skipMany hspace1

parens :: Parser a -> Parser a
parens p = start *> p <* end
  where
    start = chunk "(" *> spaceConsumer
    end = chunk ")" <* spaceConsumer

{- FOURMOLU_DISABLE -}
impliesSymbol, notSymbol, andSymbol, orSymbol :: Parser Text
impliesSymbol = (chunk "->"  <|> chunk "=>")  <* spaceConsumer
notSymbol     = (chunk "not" <|> chunk "~")   <* spaceConsumer
andSymbol     = (chunk "and" <|> chunk "/\\") <* spaceConsumer
orSymbol      = (chunk "or"  <|> chunk "\\/") <* spaceConsumer
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
top, bot, var :: Parser Formula
top = try $ Top    <$  chunk "top"                                <* spaceConsumer
bot = try $ Bottom <$  chunk "bot"                                <* spaceConsumer
var = Var         <$> takeWhile1P (Just "variable name") isAlpha <* spaceConsumer
{- FOURMOLU_ENABLE -}

simpleExpr :: Parser Formula
simpleExpr = choice [parens newLineAnd, top, bot, var]

notExpr :: Parser Formula
notExpr = (notSymbol *> (Not <$> simpleExpr)) <|> simpleExpr

wrapOr :: Formula -> Parser Formula
wrapOr f = (p >>= wrapOr) <|> pure f
  where
    p = Or f <$> (orSymbol *> notExpr)

orExpr :: Parser Formula
orExpr = notExpr >>= wrapOr

wrapAnd :: Formula -> Parser Formula
wrapAnd f = (p >>= wrapAnd) <|> pure f
  where
    p = And f <$> (andSymbol *> orExpr)

andExpr :: Parser Formula
andExpr = orExpr >>= wrapAnd

wrapImply :: Formula -> Parser Formula
wrapImply f = (p >>= wrapImply) <|> pure f
  where
    p = Implies f <$> (impliesSymbol *> andExpr)

implyExpr :: Parser Formula
implyExpr = andExpr >>= wrapImply

-- try here because is a newline might not have a rhs (trailing newline)
wrapNewlineAnd :: Formula -> Parser Formula
wrapNewlineAnd f = (try p >>= wrapNewlineAnd) <|> pure f
  where
    p = And f <$> (some eol *> spaceConsumer *> implyExpr)

newLineAnd :: Parser Formula
newLineAnd = implyExpr >>= wrapNewlineAnd

pFormula :: Parser Formula
pFormula = many eol *> newLineAnd <* many eol <* eof
