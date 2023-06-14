-- Grafo_Nodos_conectados_en_un_grafo.hs
-- TAD de los grafos: Nodos conectados en un grafo.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 21-junio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir la función,
--    conectados :: Grafo Int Int -> Int -> Int -> Bool
-- tal que (conectados g v1 v2) se verifica si los vértices v1 y v2
-- están conectados en el grafo g. Por ejemplo, si grafo1 es el grafo
-- definido por
--    grafo1 :: Grafo Int Int
--    grafo1 = creaGrafo' D (1,6) [(1,3),(1,5),(3,5),(5,1),(5,50),
--                                 (2,4),(2,6),(4,6),(4,4),(6,4)]
-- entonces,
--    conectados grafo1 1 3  ==  True
--    conectados grafo1 1 4  ==  False
--    conectados grafo1 6 2  ==  False
--    conectados grafo1 3 1  ==  True
-- ----------------------------------------------------------------------------

module Grafo_Nodos_conectados_en_un_grafo where

import TAD.Grafo (Grafo, Orientacion (D, ND), adyacentes, creaGrafo')
import Data.List (union)
import Test.Hspec (Spec, hspec, it, shouldBe)

conectados :: Grafo Int Int -> Int -> Int -> Bool
conectados g v1 v2 = v2 `elem` conectadosAux g [] [v1]

conectadosAux :: Grafo Int Int -> [Int] -> [Int] -> [Int]
conectadosAux _ vs [] = vs
conectadosAux g vs (w:ws)
  | w `elem` vs = conectadosAux g vs ws
  | otherwise = conectadosAux g ([w] `union` vs) (ws `union` adyacentes g w)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    conectados grafo1 1 3  `shouldBe`  True
  it "e2" $
    conectados grafo1 1 4  `shouldBe`  False
  it "e3" $
    conectados grafo1 6 2  `shouldBe`  False
  it "e4" $
    conectados grafo1 3 1  `shouldBe`  True
  it "e5" $
    conectados grafo2 1 3  `shouldBe`  True
  it "e6" $
    conectados grafo2 1 4  `shouldBe`  False
  it "e7" $
    conectados grafo2 6 2  `shouldBe`  True
  it "e8" $
    conectados grafo2 3 1  `shouldBe`  True
  where
    grafo1, grafo2 :: Grafo Int Int
    grafo1 = creaGrafo' D (1,6) [(1,3),(1,5),(3,5),(5,1),(5,50),
                                 (2,4),(2,6),(4,6),(4,4),(6,4)]

    grafo2 = creaGrafo' ND (1,6) [(1,3),(1,5),(3,5),(5,1),(5,50),
                                  (2,4),(2,6),(4,6),(4,4),(6,4)]

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
--
--    Finished in 0.0032 seconds
--    8 examples, 0 failures
