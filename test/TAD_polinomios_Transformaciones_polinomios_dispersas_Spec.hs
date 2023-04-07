module TAD_polinomios_Transformaciones_polinomios_dispersas_Spec (main, spec) where

import TAD_polinomios_Transformaciones_polinomios_dispersas
import TAD.Polinomio
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "e1" $
    show (dispersaApolinomio [(6,9),(3,5),(1,4),(0,7)])
    `shouldBe` "9*x^6 + 5*x^3 + 4*x + 7"
  it "e2" $
    show (dispersaApolinomio2 [(6,9),(3,5),(1,4),(0,7)])
    `shouldBe` "9*x^6 + 5*x^3 + 4*x + 7"
  it "e3" $
    show (dispersaApolinomio3 [(6,9),(3,5),(1,4),(0,7)])
    `shouldBe` "9*x^6 + 5*x^3 + 4*x + 7"
  it "e4" $
    polinomioAdispersa (consPol 6 9 (consPol 3 5 (consPol 1 4 (consPol 0 7 polCero))))
    `shouldBe` [(6,9),(3,5),(1,4),(0,7)]
