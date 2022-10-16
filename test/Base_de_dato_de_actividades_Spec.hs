module Base_de_dato_de_actividades_Spec (main, spec) where

import Base_de_dato_de_actividades
import Test.Hspec

main :: IO ()
main = hspec spec

specG1 :: ([(String,String,Int,Int)] -> [String]) -> Spec
specG1 nombres1 =
  it "e1" $
    nombres1 personas `shouldBe`
      ["Cervantes","Velazquez","Picasso","Beethoven","Poincare",
       "Quevedo","Goya","Einstein","Mozart","Botticelli","Borromini",
       "Bach"]

specG2 :: ([(String,String,Int,Int)] -> [String]) -> Spec
specG2 musicos1 =
  it "e1" $
    musicos1 personas `shouldBe`  ["Beethoven","Mozart","Bach"]

specG3 :: ([(String,String,Int,Int)] -> String -> [String]) -> Spec
specG3 seleccion1 = do
  it "e1" $
    seleccion1 personas "Pintura" `shouldBe`
      ["Velazquez","Picasso","Goya","Botticelli"]
  it "e2" $
    seleccion1 personas "Musica" `shouldBe`
      ["Beethoven","Mozart","Bach"]

specG4 :: ([(String,String,Int,Int)] -> [String]) -> Spec
specG4 musicos1' =
  it "e1" $
    musicos1' personas `shouldBe`  ["Beethoven","Mozart","Bach"]

specG5 :: ([(String,String,Int,Int)] -> Int -> [String]) -> Spec
specG5 vivas1 =
  it "e1" $
    vivas1 personas 1600 `shouldBe`
      ["Cervantes","Velazquez","Quevedo","Borromini"]

spec :: Spec
spec = do
  describe "def. 1" $ specG1 nombres
  describe "def. 1" $ specG2 musicos
  describe "def. 1" $ specG3 seleccion
  describe "def. 1" $ specG4 musicos'
  describe "def. 1" $ specG5 vivas
