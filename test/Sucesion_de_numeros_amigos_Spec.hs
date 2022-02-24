module Sucesion_de_numeros_amigos_Spec (main, spec) where

import Sucesion_de_numeros_amigos
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: [(Integer,Integer)] -> Spec
specG sucesionAmigos = do
  it "e1" $
    head sucesionAmigos `shouldBe` (220,284)

spec :: Spec
spec = do
  describe "def. 1" $ specG sucesionAmigos1
  describe "def. 2" $ specG sucesionAmigos2
  describe "def. 3" $ specG sucesionAmigos3
  describe "def. 4" $ specG sucesionAmigos4
  describe "def. 5" $ specG sucesionAmigos5
  describe "def. 6" $ specG sucesionAmigos6
  describe "def. 7" $ specG sucesionAmigos7
