module Huecos_maximales_entre_primos_Spec (main, spec) where

import Huecos_maximales_entre_primos
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: ([(Integer,Integer)]) -> Spec
specG primosYhuecosMaximales = do
  it "e1" $
    take 8 primosYhuecosMaximales `shouldBe`
    [(2,1),(3,2),(7,4),(23,6),(89,8),(113,14),(523,18),(887,20)]

spec :: Spec
spec = do
  describe "def. 1" $ specG primosYhuecosMaximales1
  describe "def. 2" $ specG primosYhuecosMaximales2
  describe "def. 3" $ specG primosYhuecosMaximales3
