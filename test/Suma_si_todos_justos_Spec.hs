module Suma_si_todos_justos_Spec (main, spec) where

import Suma_si_todos_justos
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([Maybe Integer] -> Maybe Integer) -> Spec
specG sumaSiTodosJustos = do
  it "e1" $
    sumaSiTodosJustos [Just 2, Just 5]           `shouldBe` Just 7
  it "e2" $
    sumaSiTodosJustos [Just 2, Just 5, Nothing]  `shouldBe` Nothing

spec :: Spec
spec = do
  describe "def. 1" $ specG sumaSiTodosJustos1
  describe "def. 2" $ specG sumaSiTodosJustos2
  describe "def. 3" $ specG sumaSiTodosJustos3
  describe "def. 4" $ specG sumaSiTodosJustos4
  describe "def. 5" $ specG sumaSiTodosJustos5
  describe "def. 6" $ specG sumaSiTodosJustos6
  describe "def. 7" $ specG sumaSiTodosJustos7
