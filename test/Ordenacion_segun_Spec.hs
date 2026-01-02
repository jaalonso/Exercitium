module Ordenacion_segun_Spec (main, spec) where

import Ordenacion_segun (spec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec spec
