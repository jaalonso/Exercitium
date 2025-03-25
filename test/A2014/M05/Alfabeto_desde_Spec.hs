module A2014.M05.Alfabeto_desde_Spec (main, spec) where

import A2014.M05.Alfabeto_desde (spec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec spec
