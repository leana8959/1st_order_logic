module Main where

import ParserSpec
import Test.Hspec

main :: IO ()
main = hspec $
  do
    ParserSpec.spec
