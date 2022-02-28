module Matriz_Toeplitz_Spec (main, spec) where

import Matriz_Toeplitz
import Data.Array
import Test.Hspec

main :: IO ()
main = hspec spec

specG :: (Array (Int,Int) Int -> Bool) -> Spec
specG esToeplitz = do
  it "e1" $
    esToeplitz ej1  `shouldBe`  True
  it "e2" $
    esToeplitz ej2  `shouldBe`  False

spec :: Spec
spec = do
  describe "def. 1" $ specG esToeplitz1
  describe "def. 2" $ specG esToeplitz2
