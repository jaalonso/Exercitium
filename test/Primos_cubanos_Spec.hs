module Primos_cubanos_Spec (main, spec) where

import Primos_cubanos
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: ([Integer]) -> Spec
specG cubanos = do
  it "e1" $
    take 15 cubanos `shouldBe`
    [7,19,37,61,127,271,331,397,547,631,919,1657,1801,1951,2269]
    
spec :: Spec
spec = do
  describe "def. 1" $ specG cubanos1
  describe "def. 2" $ specG cubanos2
  describe "def. 3" $ specG cubanos3
  describe "equivalencia" $ it "p1" $ property prop_cubanos
