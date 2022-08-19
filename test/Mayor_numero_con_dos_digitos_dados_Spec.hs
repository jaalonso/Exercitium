module Mayor_numero_con_dos_digitos_dados_Spec (main, spec) where

import Mayor_numero_con_dos_digitos_dados
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Int -> Int -> Int) -> Spec
specG numeroMayor = do
  it "e1" $
    numeroMayor 2 5 `shouldBe`  52
  it "e2" $
    numeroMayor 5 2 `shouldBe`  52

spec :: Spec
spec = do
  describe "def. 1" $ specG numeroMayor1
  describe "def. 2" $ specG numeroMayor2
