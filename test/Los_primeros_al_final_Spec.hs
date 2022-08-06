module Los_primeros_al_final_Spec (main, spec) where

import Los_primeros_al_final
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    rota 1 [3,2,5,7]  `shouldBe`  [2,5,7,3]
  it "e2" $
    rota 2 [3,2,5,7]  `shouldBe`  [5,7,3,2]
  it "e3" $
    rota 3 [3,2,5,7]  `shouldBe`  [7,3,2,5]
