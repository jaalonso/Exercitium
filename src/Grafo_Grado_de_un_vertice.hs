-- Grafo_Grado_de_un_vertice.hs
-- TAD de los grafos: Grado de un vértice.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 6-junio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El grado de un vértice v de un grafo dirigido g, es el número de
-- aristas de g que contiene a v. Si g es no dirigido, el grado de un
-- vértice v es el número de aristas incidentes en v, teniendo en cuenta
-- que los lazos se cuentan dos veces.
--
-- Usando el [tipo abstrado de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir las funciones,
--    grado :: (Ix v,Num p) => Grafo v p -> v -> Int
-- tal que (grado g v) es el grado del vértice v en el grafo g. Por
-- ejemplo,
--    grado g1 5  ==  4
--    grado g2 5  ==  3
--    grado g2 1  ==  3
--    grado g3 2  ==  4
--    grado g3 1  ==  2
--    grado g3 3  ==  2
--    grado g5 1  ==  2
--    grado g10 3 ==  4
--    grado g11 3 ==  4
--
-- Comprobar con QuickCheck que en todo grafo, el número de nodos de
-- grado impar es par.
-- ---------------------------------------------------------------------

module Grafo_Grado_de_un_vertice where

import TAD.Grafo (Grafo, Orientacion (D, ND), dirigido, nodos, creaGrafo')
import Data.Ix (Ix)
import Grafo_Lazos_de_un_grafo (lazos)
import Grafo_Incidentes_de_un_vertice (incidentes)
import Grafo_Grados_positivos_y_negativos (gradoPos, gradoNeg)
import Test.Hspec (Spec, hspec, it, shouldBe)

grado :: (Ix v,Num p) => Grafo v p -> v -> Int
grado g v | dirigido g           = gradoNeg g v + gradoPos g v
          | (v,v) `elem` lazos g = length (incidentes g v) + 1
          | otherwise            = length (incidentes g v)

-- La propiedad es
prop_numNodosGradoImpar :: Grafo Int Int -> Bool
prop_numNodosGradoImpar g =
  even (length [v | v <- nodos g, odd (grado g v)])

-- La comprobación es
--    λ> quickCheck prop_numNodosGradoImpar
--    +++ OK, passed 100 tests.

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    grado g1 5 `shouldBe`  4
  it "e2" $
    grado g2 5 `shouldBe`  3
  it "e3" $
    grado g2 1 `shouldBe`  3
  it "e4" $
    grado g3 2 `shouldBe`  4
  it "e5" $
    grado g3 1 `shouldBe`  2
  it "e6" $
    grado g3 3 `shouldBe`  2
  it "e7" $
    grado g4 1 `shouldBe`  2
  it "e8" $
    grado g5 3 `shouldBe`  4
  it "e9" $
    grado g6 3 `shouldBe`  4
  where
    g1, g2, g3, g4, g5, g6 :: Grafo Int Int
    g1 = creaGrafo' ND (1,5) [(1,2),(1,3),(1,5),(2,4),(2,5),(3,4),(3,5),(4,5)]
    g2 = creaGrafo' D  (1,5) [(1,2),(1,3),(1,5),(2,4),(2,5),(4,3),(4,5)]
    g3 = creaGrafo' D  (1,3) [(1,2),(2,2),(3,1),(3,2)]
    g4 = creaGrafo' D  (1,1) [(1,1)]
    g5 = creaGrafo' ND (1,3) [(1,2),(1,3),(2,3),(3,3)]
    g6 = creaGrafo' D  (1,3) [(1,2),(1,3),(2,3),(3,3)]

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--    e4
--    e5
--    e6
--    e7
--    e8
--    e9
--
--    Finished in 0.0015 seconds
--    9 examples, 0 failures
