module Iguales_al_siguiente_Spec (main, spec) where

import Iguales_al_siguiente
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "suma" $ do
    it "e1" $
      igualesAlSiguiente1 [1,2,2,2,3,3,4] `shouldBe` [2,2,3]
    it "e2" $
      igualesAlSiguiente1 [1..10] `shouldBe` []
    it "e3" $
      property prop_igualesAlSiguiente
