module Mayor_rectangulo_Spec (main, spec) where

import Mayor_rectangulo
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    mayorRectangulo (4,6) (3,7)  `shouldBe`  (4,6)
  it "e2" $
    mayorRectangulo (4,6) (3,8)  `shouldBe`  (4,6)
  it "e3" $
    mayorRectangulo (4,6) (3,9)  `shouldBe`  (3,9)
