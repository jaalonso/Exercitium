module TAD_Particion_segun_un_numero_Spec (main, spec) where

import TAD_Particion_segun_un_numero
import TAD.Conjunto
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Int -> Conj Int -> (Conj Int, Conj Int)) -> Spec
specG divide' = do
  it "e1" $
    show (divide' 5 (inserta 7 (inserta 2 (inserta 8 vacio))))
    `shouldBe` "({2},{7, 8})"

spec :: Spec
spec = do
  describe "def. 1" $ specG divide
  describe "def. 2" $ specG divide2
  describe "equivalencia" $ it "p1" $ property prop_divide
