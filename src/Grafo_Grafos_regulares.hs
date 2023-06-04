-- Grafo_Grafos_regulares.hs
-- TAD de los grafos: Grafos regulares.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 8-junio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un grafo es regular si todos sus vértices tienen el mismo
-- grado.
--
-- Usando el [tipo abstracto de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir la función,
--    regular :: (Ix v,Num p) => Grafo v p -> Bool
-- tal que (regular g) se verifica si el grafo g es regular. Por ejemplo,
--    λ> regular (creaGrafo' D (1,3) [(1,2),(2,3),(3,1)])
--    True
--    λ> regular (creaGrafo' ND (1,3) [(1,2),(2,3)])
--    False
--    λ> regular (completo 4)
--    True
--
-- Comprobar que los grafos completos son regulares.
-- ---------------------------------------------------------------------

module Grafo_Grafos_regulares where

import TAD.Grafo (Grafo, Orientacion (D, ND), nodos, creaGrafo')
import Data.Ix (Ix)
import Grafo_Grado_de_un_vertice (grado)
import Grafo_Grafos_completos (completo)
import Test.Hspec (Spec, hspec, it, shouldBe)

regular :: (Ix v,Num p) => Grafo v p -> Bool
regular g = and [grado g v == k | v <- vs]
  where vs = nodos g
        k  = grado g (head vs)

-- La propiedad de la regularidad de todos los grafos completos de orden
-- entre m y n es
prop_CompletoRegular :: Int -> Int -> Bool
prop_CompletoRegular m n =
  and [regular (completo x) | x <- [m..n]]

-- La comprobación es
--    λ> prop_CompletoRegular 1 30
--    True

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    regular g1 `shouldBe` True
  it "e2" $
    regular g2 `shouldBe` False
  it "e3" $
    regular (completo 4) `shouldBe` True
  where
    g1, g2 :: Grafo Int Int
    g1 = creaGrafo' D (1,3) [(1,2),(2,3),(3,1)]
    g2 = creaGrafo' ND (1,3) [(1,2),(2,3)]

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--
--    Finished in 0.0006 seconds
--    3 examples, 0 failures
