module El_tipo_de_los_numeros_naturales_Spec (main, spec) where

import El_tipo_de_los_numeros_naturales
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    nat2int (Suc (Suc (Suc Cero)))  `shouldBe`  3
  it "e2" $
    int2nat 3 `shouldBe`  Suc (Suc (Suc Cero))
  it "e3" $
    suma (Suc (Suc Cero)) (Suc Cero) `shouldBe`
      Suc (Suc (Suc Cero))
  it "e4" $
    nat2int (suma (Suc (Suc Cero)) (Suc Cero)) `shouldBe` 3
  it "e5" $
    nat2int (suma (int2nat 2) (int2nat 1)) `shouldBe` 3
