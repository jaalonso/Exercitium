-- Grafo_Numero_de_aristas_de_un_grafo.hs
-- TAD de los grafos: Número de aristas de un grafo.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 1-junio-2023
-- ======================================================================

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir la función,
--    nAristas :: (Ix v,Num p) => Grafo v p ->  Int
-- tal que (nAristas g) es el número de aristas del grafo g. Si g es no
-- dirigido, las aristas de v1 a v2 y de v2 a v1 sólo se cuentan una
-- vez. Por ejemplo,
--    λ> g1 = creaGrafo' ND (1,5) [(1,2),(1,3),(1,5),(2,4),(2,5),(3,4),(3,5),(4,5)]
--    λ> g2 = creaGrafo' D  (1,5) [(1,2),(1,3),(1,5),(2,4),(2,5),(4,3),(4,5)]
--    λ> g3 = creaGrafo' ND (1,3) [(1,2),(1,3),(2,3),(3,3)]
--    λ> g4 = creaGrafo' ND (1,4) [(1,1),(1,2),(3,3)]
--    λ> nAristas g1
--    8
--    λ> nAristas g2
--    7
--    λ> nAristas g3
--    4
--    λ> nAristas g4
--    3
--    λ> nAristas (completo 4)
--    6
--    λ> nAristas (completo 5)
--    10
--
-- Definir la función
--    prop_nAristasCompleto :: Int -> Bool
-- tal que (prop_nAristasCompleto n) se verifica si el número de aristas
-- del grafo completo de orden n es n*(n-1)/2 y, usando la función,
-- comprobar que la propiedad se cumple para n de 1 a 20.
-- ---------------------------------------------------------------------

module Grafo_Numero_de_aristas_de_un_grafo where

import TAD.Grafo (Grafo, Orientacion (D, ND), dirigido, aristas, creaGrafo')
import Data.Ix (Ix)
import Grafo_Lazos_de_un_grafo (nLazos)
import Grafo_Grafos_completos (completo)
import Test.Hspec (Spec, hspec, it, shouldBe, describe)

-- 1ª solución
-- ===========

nAristas :: (Ix v,Num p) => Grafo v p ->  Int
nAristas g | dirigido g = length (aristas g)
           | otherwise  = (length (aristas g) + nLazos g) `div` 2

-- 2ª solución
-- ===========

nAristas2 :: (Ix v,Num p) => Grafo v p ->  Int
nAristas2 g | dirigido g = length (aristas g)
            | otherwise  = length [(x,y) | ((x,y),_) <- aristas g, x <= y]

-- Propiedad
-- =========

prop_nAristasCompleto :: Int -> Bool
prop_nAristasCompleto n =
  nAristas (completo n) == n*(n-1) `div` 2

-- La comprobación es
--    λ> and [prop_nAristasCompleto n | n <- [1..20]]
--    True

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  describe "def. 1" $ specG nAristas
  describe "def. 2" $ specG nAristas2

specG :: (Grafo Int Int -> Int) -> Spec
specG nAristas' = do
  it "e1" $
    nAristas' g1 `shouldBe` 8
  it "e2" $
    nAristas' g2 `shouldBe` 7
  it "e3" $
    nAristas' g3 `shouldBe` 4
  it "e4" $
    nAristas' g4 `shouldBe` 3
  it "e5" $
    nAristas' (completo 4) `shouldBe` 6
  it "e6" $
    nAristas' (completo 5) `shouldBe` 10
  where
    g1, g2, g3, g4 :: Grafo Int Int
    g1 = creaGrafo' ND (1,5) [(1,2),(1,3),(1,5),(2,4),(2,5),(3,4),(3,5),(4,5)]
    g2 = creaGrafo' D  (1,5) [(1,2),(1,3),(1,5),(2,4),(2,5),(4,3),(4,5)]
    g3 = creaGrafo' ND (1,3) [(1,2),(1,3),(2,3),(3,3)]
    g4 = creaGrafo' ND (1,4) [(1,1),(1,2),(3,3)]

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
