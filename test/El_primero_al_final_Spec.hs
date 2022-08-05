module El_primero_al_final_Spec (main, spec) where

import El_primero_al_final
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    rota1 [3,2,5,7]  `shouldBe`  [2,5,7,3]
