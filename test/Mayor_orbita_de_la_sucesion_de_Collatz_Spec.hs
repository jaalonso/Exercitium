module Mayor_orbita_de_la_sucesion_de_Collatz_Spec (main, spec) where

import Mayor_orbita_de_la_sucesion_de_Collatz
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Integer -> [Integer]) -> Spec
specG mayoresGeneradores' = do
  it "e1" $
    mayoresGeneradores' 20 `shouldBe` [18,19]

spec :: Spec
spec = do
  describe "def. 1" $ specG mayoresGeneradores
  describe "def. 2" $ specG mayoresGeneradores2
  describe "def. 3" $ specG mayoresGeneradores3
