module ParserSpec where

import Test.Hspec
import Text.Megaparsec

import Parser
import Types

(=?) input expect =
  case runParser pFormula "<test>" input of
    Right x -> x `shouldBe` expect
    Left err -> expectationFailure $ "Parser error :\n" <> errorBundlePretty err

spec :: Spec
spec =
  describe "Parser" do
    it "should parse implies" do
      "a -> b" =? Implies (Var "a") (Var "b")
      "a => b" =? Implies (Var "a") (Var "b")
    it "should parse and" do
      "a and b" =? And (Var "a") (Var "b")
      "a /\\ b" =? And (Var "a") (Var "b")
    it "should parse or" do
      "a or b" =? Or (Var "a") (Var "b")
      "a \\/ b" =? Or (Var "a") (Var "b")
    it "should respect precedence and parentheses" do
      "a or (b and c)" =? Or (Var "a") (And (Var "b") (Var "c"))
      "a or b and c" =? And (Or (Var "a") (Var "b")) (Var "c")
      "a and (b or c)" =? And (Var "a") (Or (Var "b") (Var "c"))
      "a and b or c" =? And (Var "a") (Or (Var "b") (Var "c"))
      "(a and b) or c" =? Or (And (Var "a") (Var "b")) (Var "c")
    it "and should be associative" do
      "a and b and c" =? And (And (Var "a") (Var "b")) (Var "c")
    it "or should be associative" do
      "a or b or c" =? Or (Or (Var "a") (Var "b")) (Var "c")
    it "should pass more tests hehe" do
      "p \\/ q \\/ r and (p -> ~q) and (q -> ~r) and (r -> ~p)"
        =? And
          ( And
              ( And
                  (Or (Or (Var "p") (Var "q")) (Var "r"))
                  (Implies (Var "p") (Not (Var "q")))
              )
              (Implies (Var "q") (Not (Var "r")))
          )
          (Implies (Var "r") (Not (Var "p")))
    it "should handle linebreak" do
      unlines ["a", "b", "c"] =? And (And (Var "a") (Var "b")) (Var "c")
      unlines
        [ "p \\/ q \\/ r"
        , "p -> (~q)"
        , "q -> (~r)"
        , "r -> (~p)"
        , ""
        ]
        =? And
          ( And
              ( And
                  (Or (Or (Var "p") (Var "q")) (Var "r"))
                  (Implies (Var "p") (Not (Var "q")))
              )
              (Implies (Var "q") (Not (Var "r")))
          )
          (Implies (Var "r") (Not (Var "p")))
    it "should not halt where multiple empty lines are present" do
      unlines
        [ "p \\/ q \\/ r"
        , ""
        , "p -> (~q)"
        , ""
        , ""
        , ""
        , "q -> (~r)"
        , "r -> (~p)"
        , ""
        ]
        =? And
          ( And
              ( And
                  (Or (Or (Var "p") (Var "q")) (Var "r"))
                  (Implies (Var "p") (Not (Var "q")))
              )
              (Implies (Var "q") (Not (Var "r")))
          )
          (Implies (Var "r") (Not (Var "p")))
