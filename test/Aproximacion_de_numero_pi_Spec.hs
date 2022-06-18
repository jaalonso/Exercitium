module Aproximacion_de_numero_pi_Spec (main, spec) where

import Aproximacion_de_numero_pi
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

specG :: (Integer -> Double) -> Spec
specG aproximaPi = do
  it "e1" $
   aproximaPi 10 =~ 3.1411060206
  it "e2" $
   aproximaPi 20 =~ 3.1415922987403397
  it "e3" $
   aproximaPi 30 =~ 3.1415926533011596
  it "e4" $
   aproximaPi 40 =~ 3.1415926535895466
  it "e5" $
   aproximaPi 50 =~ 3.141592653589793

spec :: Spec
spec = do
  describe "def. 1" $ specG aproximaPi1
  describe "def. 2" $ specG aproximaPi2
  describe "def. 3" $ specG aproximaPi3
  describe "def. 4" $ specG aproximaPi4
  describe "equivalencia" $ it "p1" $ property prop_aproximaPi
