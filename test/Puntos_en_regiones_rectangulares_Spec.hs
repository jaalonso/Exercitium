module Puntos_en_regiones_rectangulares_Spec (main, spec) where

import Puntos_en_regiones_rectangulares
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Punto -> Region -> Bool) -> Spec
specG enRegion' = do
  it "e1" $
    enRegion' (1,0) r0021                                  `shouldBe`  True
  it "e2" $
    enRegion' (3,0) r0021                                  `shouldBe`  False
  it "e3" $
    enRegion' (1,1) (Union r0021 r3051)                     `shouldBe`  True
  it "e4" $
    enRegion' (4,0) (Union r0021 r3051)                     `shouldBe`  True
  it "e5" $
    enRegion' (4,2) (Union r0021 r3051)                     `shouldBe`  False
  it "e6" $
    enRegion' (3,1) (Diferencia r3051 r4162)                `shouldBe`  True
  it "e7" $
    enRegion' (4,1) (Diferencia r3051 r4162)                `shouldBe`  False
  it "e8" $
    enRegion' (4,2) (Diferencia r3051 r4162)                `shouldBe`  False
  it "e9" $
    enRegion' (4,2) (Union (Diferencia r3051 r4162) r4162)  `shouldBe`  True

spec :: Spec
spec = do
  describe "def. 1" $ specG enRegion
