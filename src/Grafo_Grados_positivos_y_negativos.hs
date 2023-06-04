-- Grafo_Grados_positivos_y_negativos.hs
-- TAD de los grafos: Grados_positivos_y_negativos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 2-junio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El grado positivo de un vértice v de un grafo g es el número de
-- vértices de g adyacentes con v y su grado negativo es el número de
-- vértices de g incidentes con v.
--
-- Usando el [tipo abstracto de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir las funciones,
--    gradoPos :: (Ix v,Num p) => Grafo v p -> v -> Int
--    gradoNeg :: (Ix v,Num p) => Grafo v p -> v -> Int
-- tales que
-- + (gradoPos g v) es el grado positivo del vértice v en el grafo g.
--   Por ejemplo,
--      λ> g1 = creaGrafo' ND (1,5) [(1,2),(1,3),(1,5),(2,4),(2,5),(3,4),(3,5),(4,5)]
--      λ> g2 = creaGrafo' D  (1,5) [(1,2),(1,3),(1,5),(2,4),(2,5),(4,3),(4,5)]
--      λ> gradoPos g1 5
--      4
--      λ> gradoPos g2 5
--      0
--      λ> gradoPos g2 1
--      3
-- + (gradoNeg g v) es el grado negativo del vértice v en el grafo g.
--   Por ejemplo,
--      λ> gradoNeg g1 5
--      4
--      λ> gradoNeg g2 5
--      3
--      λ> gradoNeg g2 1
--      0
-- ---------------------------------------------------------------------

module Grafo_Grados_positivos_y_negativos where

import TAD.Grafo (Grafo, Orientacion (D, ND), adyacentes, creaGrafo')
import Data.Ix (Ix)
import Grafo_Incidentes_de_un_vertice (incidentes)
import Test.Hspec (Spec, hspec, it, shouldBe)

-- 1ª definición de gradoPos
gradoPos :: (Ix v,Num p) => Grafo v p -> v -> Int
gradoPos g v = length (adyacentes g v)

-- 2ª definición de gradoPos
gradoPos2 :: (Ix v,Num p) => Grafo v p -> v -> Int
gradoPos2 g = length . adyacentes g

-- 1ª definición de gradoNeg
gradoNeg :: (Ix v,Num p) => Grafo v p -> v -> Int
gradoNeg g v = length (incidentes g v)

-- 2ª definición de gradoNeg
gradoNeg2 :: (Ix v,Num p) => Grafo v p -> v -> Int
gradoNeg2 g = length . incidentes g

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    gradoPos g1 5 `shouldBe` 4
  it "e2" $
    gradoPos g2 5 `shouldBe` 0
  it "e3" $
    gradoPos g2 1 `shouldBe` 3
  it "e4" $
    gradoNeg g1 5 `shouldBe` 4
  it "e5" $
    gradoNeg g2 5 `shouldBe` 3
  it "e6" $
    gradoNeg g2 1 `shouldBe` 0
  it "e7" $
    gradoPos2 g1 5 `shouldBe` 4
  it "e8" $
    gradoPos2 g2 5 `shouldBe` 0
  it "e9" $
    gradoPos2 g2 1 `shouldBe` 3
  it "e10" $
    gradoNeg2 g1 5 `shouldBe` 4
  it "e11" $
    gradoNeg2 g2 5 `shouldBe` 3
  it "e12" $
    gradoNeg2 g2 1 `shouldBe` 0
  where
    g1, g2 :: Grafo Int Int
    g1 = creaGrafo' ND (1,5) [(1,2),(1,3),(1,5),(2,4),(2,5),(3,4),(3,5),(4,5)]
    g2 = creaGrafo' D  (1,5) [(1,2),(1,3),(1,5),(2,4),(2,5),(4,3),(4,5)]

-- La verificación es
--    λ> verifica
--
--    def. 1
--      e1
--      e2
--      e3
--      e4
--      e5
--      e6
--    def. 2
--      e1
--      e2
--      e3
--      e4
--      e5
--      e6
--
--    Finished in 0.0013 seconds
--    12 examples, 0 failures
