module TAD_polinomios_Transformaciones_entre_las_representaciones_dispersa_y_densa_Spec (main, spec) where

import TAD_polinomios_Transformaciones_entre_las_representaciones_dispersa_y_densa
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    densaAdispersa [9,0,0,5,0,4,7] `shouldBe` [(6,9),(3,5),(1,4),(0,7)]
  it "e2" $
    densaAdispersa2 [9,0,0,5,0,4,7] `shouldBe` [(6,9),(3,5),(1,4),(0,7)]
  it "e3" $
    dispersaAdensa [(6,9),(3,5),(1,4),(0,7)] `shouldBe` [9,0,0,5,0,4,7]
  it "e4" $
    dispersaAdensa2 [(6,9),(3,5),(1,4),(0,7)] `shouldBe` [9,0,0,5,0,4,7]
