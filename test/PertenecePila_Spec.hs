module PertenecePila_Spec (main, spec) where

import PertenecePila
import TAD.Pila
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Int -> Pila Int -> Bool) -> Spec
specG pertenecePila' = do
  it "e1" $
    pertenecePila' 2 (apila 5 (apila 2 (apila 3 vacia))) == True
  it "e2" $
    pertenecePila' 4 (apila 5 (apila 2 (apila 3 vacia))) == False

spec :: Spec
spec = do
  describe "def. 1" $ specG pertenecePila
  describe "def. 2" $ specG pertenecePila2
