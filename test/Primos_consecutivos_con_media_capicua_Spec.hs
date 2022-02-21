module Primos_consecutivos_con_media_capicua_Spec (main, spec) where

import Primos_consecutivos_con_media_capicua
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "e1" $
      take 5 primosConsecutivosConMediaCapicua `shouldBe`
      [(3,5,4),(5,7,6),(7,11,9),(97,101,99),(109,113,111)]
    it "e2" $
      take 5 primosConsecutivosConMediaCapicua `shouldBe`
      [(3,5,4),(5,7,6),(7,11,9),(97,101,99),(109,113,111)]
