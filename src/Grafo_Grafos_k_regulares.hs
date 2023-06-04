-- Grafo_Grafos_k_regulares.hs
-- TAD de los grafos: Grafos k-regulares.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 30-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un grafo es k-regular si todos sus vértices son de grado k.
-- Usando el [tipo abstracto de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir la función,
--    regularidad :: (Ix v,Num p) => Grafo v p -> Maybe Int
-- tal que (regularidad g) es la regularidad de g. Por ejemplo,
--    regularidad (creaGrafo' ND (1,2) [(1,2),(2,3)]) == Just 1
--    regularidad (creaGrafo' D (1,2) [(1,2),(2,3)])  == Nothing
--    regularidad (completo 4)                        == Just 3
--    regularidad (completo 5)                        == Just 4
--    regularidad (grafoCiclo 4)                      == Just 2
--    regularidad (grafoCiclo 5)                      == Just 2
--
-- Comprobar que el grafo completo de orden n es (n-1)-regular (para
-- n de 1 a 20) y el grafo ciclo de orden n es 2-regular (para n de 3 a
-- 20).
-- ---------------------------------------------------------------------

module Grafo_Grafos_k_regulares where

import TAD.Grafo (Grafo, Orientacion (D, ND), nodos, creaGrafo')
import Data.Ix (Ix)
import Grafo_Grado_de_un_vertice (grado)
import Grafo_Grafos_regulares (regular)
import Grafo_Grafos_completos (completo)
import Grafo_Grafos_ciclos (grafoCiclo)
import Test.Hspec (Spec, hspec, it, shouldBe)

regularidad :: (Ix v,Num p) => Grafo v p -> Maybe Int
regularidad g
  | regular g = Just (grado g (head (nodos g)))
  | otherwise = Nothing

-- La propiedad de k-regularidad de los grafos completos es
prop_completoRegular :: Int -> Bool
prop_completoRegular n =
  regularidad (completo n) == Just (n-1)

-- La comprobación es
--    λ> and [prop_completoRegular n | n <- [1..20]]
--    True

-- La propiedad de k-regularidad de los grafos ciclos es
prop_cicloRegular :: Int -> Bool
prop_cicloRegular n =
  regularidad (grafoCiclo n) == Just 2

-- La comprobación es
--    λ> and [prop_cicloRegular n | n <- [3..20]]
--    True

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    regularidad g1             `shouldBe` Just 1
  it "e2" $
    regularidad g2             `shouldBe` Nothing
  it "e3" $
    regularidad (completo 4)   `shouldBe` Just 3
  it "e4" $
    regularidad (completo 5)   `shouldBe` Just 4
  it "e5" $
    regularidad (grafoCiclo 4) `shouldBe` Just 2
  it "e6" $
    regularidad (grafoCiclo 5) `shouldBe` Just 2
  where
    g1, g2 :: Grafo Int Int
    g1 = creaGrafo' ND (1,2) [(1,2),(2,3)]
    g2 = creaGrafo' D (1,2) [(1,2),(2,3)]

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--    e4
--    e5
--    e6
--
--    Finished in 0.0027 seconds
--    6 examples, 0 failures
