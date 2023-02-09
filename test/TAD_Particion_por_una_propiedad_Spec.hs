module TAD_Particion_por_una_propiedad_Spec (main, spec) where

import TAD_Particion_por_una_propiedad
import TAD.Conjunto
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ((Int -> Bool) -> Conj Int -> (Conj Int, Conj Int)) -> Spec
specG particion' =
  it "e1" $
    show (particion' even ej) `shouldBe` "({2, 4},{5, 7})"
  where
    ej = inserta 5 (inserta 4 (inserta 7 (inserta 2 vacio)))

spec :: Spec
spec = do
  describe "def. 1" $ specG particion
  describe "def. 2" $ specG particion2
