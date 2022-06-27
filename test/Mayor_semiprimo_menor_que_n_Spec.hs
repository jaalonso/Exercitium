module Mayor_semiprimo_menor_que_n_Spec (main, spec) where

import Mayor_semiprimo_menor_que_n
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Integer) -> Spec
specG mayorSemiprimoMenor = do
  it "e1" $
    mayorSemiprimoMenor 27 `shouldBe`  26
  it "e2" $
    mayorSemiprimoMenor 50 `shouldBe`  49
  it "e3" $
    mayorSemiprimoMenor 49 `shouldBe`  46


spec :: Spec
spec = do
  describe "def. 1" $ specG mayorSemiprimoMenor1
  describe "def. 2" $ specG mayorSemiprimoMenor2
  describe "def. 3" $ specG mayorSemiprimoMenor3
  describe "def. 4" $ specG mayorSemiprimoMenor4
  describe "def. 5" $ specG mayorSemiprimoMenor5
  describe "equivalencia" $ it "p1" $ property prop_mayorSemiprimoMenor
