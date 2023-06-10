-- Grafo_Grafos_conexos.hs
-- TAD de los grafos: Grafos conexos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 16-junio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un grafo no dirigido G se dice conexo, si para cualquier par de
-- vértices u y v en G, existe al menos una trayectoria (una  sucesión
-- de vértices adyacentes) de u a v.
--
-- Usando el [tipo abstracto de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir la función,
--    conexo :: (Ix a, Num p, Eq p) => Grafo a p -> Bool
-- tal que (conexo g) se verifica si el grafo g es conexo. Por ejemplo,
--    conexo (creaGrafo' ND (1,3) [(1,2),(3,2)])        ==  True
--    conexo (creaGrafo' ND (1,4) [(1,2),(3,2),(4,1)])  ==  True
--    conexo (creaGrafo' ND (1,4) [(1,2),(3,4)])        ==  False
-- ---------------------------------------------------------------------

module Grafo_Grafos_conexos where

import TAD.Grafo (Grafo, Orientacion (ND), nodos, creaGrafo')
import Data.Ix (Ix)
import Grafo_Recorrido_en_anchura (recorridoEnAnchura)
import Test.Hspec (Spec, hspec, it, shouldBe)

conexo :: (Ix a, Num p, Eq p) => Grafo a p -> Bool
conexo g = length (recorridoEnAnchura i g) == n
  where xs = nodos g
        i  = head xs
        n  = length xs

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    conexo g1 `shouldBe` True
  it "e2" $
    conexo g2 `shouldBe` True
  it "e3" $
    conexo g3 `shouldBe` False
  where
    g1, g2, g3 :: Grafo Int Int
    g1 = creaGrafo' ND (1,3) [(1,2),(3,2)]
    g2 = creaGrafo' ND (1,4) [(1,2),(3,2),(4,1)]
    g3 = creaGrafo' ND (1,4) [(1,2),(3,4)]

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--
--    Finished in 0.0003 seconds
--    3 examples, 0 failures
