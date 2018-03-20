import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Data.Text.Lazy as L

import Syntax
import Lexer
import Parser

parse :: String -> [Binding]
parse s = case parseModule (L.pack s) of
  Left err -> []
  Right res -> res

main :: IO ()
main = hspec $ do
  describe "Parser basics" $ do
    it "integer" $ do
      parse "a = 1" `shouldBe` [(
        "a",
        Lit (LInt 1)
        )]

    it "boolean" $ do
      parse "a = True" `shouldBe` [(
        "a",
        Lit (LBool True)
        )]

      parse "a = False" `shouldBe` [(
        "a",
        Lit (LBool False)
        )]

    it "reserved word" $ do
      parse "if = 1" `shouldBe` []

    it "lambda" $ do
      parse "main = (a) -> a" `shouldBe` [(
        "main",
        Lam "a" (Var "a")
        )]

    it "lambda multiline" $ do
      parse "add = (x) -> x\nmain = (a) -> a" `shouldBe` [(
        "add",
        Lam "x" (Var "x")
        ),(
        "main",
        Lam "a" (Var "a")
        )]

    it "lambda multi-argument" $ do
      parse "main = (a,b,c) -> a" `shouldBe` [(
        "main",
        Lam "a" (
          Lam "b" (
            Lam "c" (Var "a")))
        )]

    it "if" $ do
      parse "main = if 1 is 1 then 0 else 1" `shouldBe` [(
        "main",
        If (Op Eql (Lit (LInt 1)) (Lit (LInt 1))) (Lit (LInt 0)) (Lit (LInt 1))
        )]
