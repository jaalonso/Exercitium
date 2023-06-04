-- Grafo_Contiguos_de_un_vertice.hs
-- TAD de los grafos: Contiguos de un vértice.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 30-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- En un un grafo g, los contiguos de un vértice v es el conjuntos de
-- vértices x de g tales que x es adyacente o incidente con v.
--
-- Usando el [tipo abstracto de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir la función,
--    contiguos :: (Ix v,Num p) => Grafo v p -> v -> [v]
-- tal que (contiguos g v) es el conjunto de los vértices de g contiguos
-- con el vértice v. Por ejemplo,
--    λ> g1 = creaGrafo' D (1,3) [(1,2),(2,2),(3,1),(3,2)]
--    λ> contiguos g1 1
--    [2,3]
--    λ> contiguos g1 2
--    [2,1,3]
--    λ> contiguos g1 3
--    [1,2]
--    λ> g2 = creaGrafo' ND (1,3) [(1,2),(2,2),(3,1),(3,2)]
--    λ> contiguos g2 1
--    [2,3]
--    λ> contiguos g2 2
--    [1,2,3]
--    λ> contiguos g2 3
--    [1,2]
-- ---------------------------------------------------------------------

module Grafo_Contiguos_de_un_vertice where

import TAD.Grafo (Grafo, Orientacion (D, ND), adyacentes, creaGrafo')
import Grafo_Incidentes_de_un_vertice (incidentes)
import Data.List (nub)
import Data.Ix
import Test.Hspec

contiguos :: (Ix v,Num p) => Grafo v p -> v -> [v]
contiguos g v = nub (adyacentes g v ++ incidentes g v)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    contiguos g1 1 `shouldBe` [2,3]
  it "e2" $
    contiguos g1 2 `shouldBe` [2,1,3]
  it "e3" $
    contiguos g1 3 `shouldBe` [1,2]
  it "e4" $
    contiguos g2 1 `shouldBe` [2,3]
  it "e5" $
    contiguos g2 2 `shouldBe` [1,2,3]
  it "e6" $
    contiguos g2 3 `shouldBe` [1,2]
  where
    g1, g2 :: Grafo Int Int
    g1 = creaGrafo' D (1,3) [(1,2),(2,2),(3,1),(3,2)]
    g2 = creaGrafo' ND (1,3) [(1,2),(2,2),(3,1),(3,2)]

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
--    Finished in 0.0005 seconds
--    6 examples, 0 failures
