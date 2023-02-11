module TAD_mapC_Spec (main, spec) where

import TAD_mapC
import TAD.Conjunto
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ((Int -> Int) -> Conj Int -> Conj Int) -> Spec
specG mapC' = do
  it "e1" $
    show (mapC' (*2) (inserta 3 (inserta 1 vacio)))
    `shouldBe` "{2, 6}"

spec :: Spec
spec = do
  describe "def. 1" $ specG mapC
  describe "def. 2" $ specG mapC2
