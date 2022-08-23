module Interseccion_de_intervalos_cerrados_Spec (main, spec) where

import Interseccion_de_intervalos_cerrados
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    interseccion [] [3,5]     `shouldBe` []
  it "e2" $
    interseccion [3,5] []     `shouldBe` []
  it "e3" $
    interseccion [2,4] [6,9]  `shouldBe` []
  it "e4" $
    interseccion [2,6] [6,9]  `shouldBe` [6,6]
  it "e5" $
    interseccion [2,6] [0,9]  `shouldBe` [2,6]
  it "e6" $
    interseccion [2,6] [0,4]  `shouldBe` [2,4]
  it "e7" $
    interseccion [4,6] [0,4]  `shouldBe` [4,4]
  it "e8" $
    interseccion [5,6] [0,4]  `shouldBe` []
