module TodosVerifican_Spec (main, spec) where

import TodosVerifican
import TAD.Cola
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ((Int -> Bool) -> Cola Int -> Bool) -> Spec
specG todosVerifican = do
  it "e1" $
    todosVerifican (>0) (inserta 3 (inserta 2 vacia))    `shouldBe` True
  it "e2" $
    todosVerifican (>0) (inserta 3 (inserta (-2) vacia)) `shouldBe` False

spec :: Spec
spec = do
  describe "def. 1" $ specG todosVerifican1
  describe "def. 2" $ specG todosVerifican2
