-- Grafo_Lazos_de_un_grafo.hs
-- TAD de los grafos: Lazos de un grafo.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 31-mayo-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir las funciones,
--    lazos  :: (Ix v,Num p) => Grafo v p -> [(v,v)]
--    nLazos :: (Ix v,Num p) => Grafo v p ->  Int
-- tales que
-- + (lazos g) es el conjunto de los lazos (es decir, aristas cuyos
--   extremos son iguales) del grafo g. Por ejemplo,
--      λ> ej1 = creaGrafo' D (1,3) [(1,1),(2,3),(3,2),(3,3)]
--      λ> ej2 = creaGrafo' ND (1,3) [(2,3),(3,1)]
--      λ> lazos ej1
--      [(1,1),(3,3)]
--      λ> lazos ej2
--      []
-- + (nLazos g) es el número de lazos del grafo g. Por ejemplo,
--      λ> nLazos ej1
--      2
--      λ> nLazos ej2
--      0
-- ---------------------------------------------------------------------

module Grafo_Lazos_de_un_grafo where

import TAD.Grafo (Grafo, Orientacion (D, ND), nodos, aristaEn, creaGrafo')
import Data.Ix (Ix)
import Test.Hspec (Spec, hspec, it, shouldBe)

lazos :: (Ix v,Num p) => Grafo v p -> [(v,v)]
lazos g = [(x,x) | x <- nodos g, aristaEn g (x,x)]

nLazos :: (Ix v,Num p) => Grafo v p ->  Int
nLazos = length . lazos

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    lazos ej1 `shouldBe` [(1,1),(3,3)]
  it "e2" $
    lazos ej2 `shouldBe` []
  it "e3" $
    nLazos ej1 `shouldBe` 2
  it "e4" $
    nLazos ej2 `shouldBe` 0
  where
    ej1, ej2 :: Grafo Int Int
    ej1 = creaGrafo' D (1,3) [(1,1),(2,3),(3,2),(3,3)]
    ej2 = creaGrafo' ND (1,3) [(2,3),(3,1)]

-- La verificación es
--      λ> verifica
--
--      e1
--      e2
--      e3
--      e4
--
--      Finished in 0.0005 seconds
--      4 examples, 0 failures
