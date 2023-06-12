-- Grafo_Nodos_aislados_de_un_grafo.hs
-- TAD de los grafos: Nodos aislados de un grafo.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-junio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Dado un grafo dirigido G, diremos que un nodo está aislado si o bien
-- de dicho nodo no sale ninguna arista o bien no llega al nodo ninguna
-- arista. Por ejemplo, en el siguiente grafo
--    grafo1 = creaGrafo D (1,6) [(1,2,0),(1,3,0),(1,4,0),(3,6,0),
--                                (5,4,0),(6,2,0),(6,5,0)]
-- podemos ver que del nodo 1 salen 3 aristas pero no llega ninguna, por
-- lo que lo consideramos aislado. Así mismo, a los nodos 2 y 4 llegan
-- aristas pero no sale ninguna, por tanto también estarán aislados.
--
-- Usando el [tipo abstracto de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir la función,
--    aislados :: (Ix v, Num p) => Grafo v p -> [v]
-- tal que (aislados g) es la lista de nodos aislados del grafo g. Por
-- ejemplo,
--    aislados grafo1 == [1,2,4]
-- ---------------------------------------------------------------------

module Grafo_Nodos_aislados_de_un_grafo where

import TAD.Grafo (Grafo, Orientacion (D), adyacentes, nodos, creaGrafo')
import Data.Ix (Ix)
import Grafo_Incidentes_de_un_vertice (incidentes)
import Test.Hspec (Spec, hspec, it, shouldBe)

grafo1 :: Grafo Int Int
grafo1 = creaGrafo' D (1,6) [(1,2),(1,3),(1,4),(3,6),
                             (5,4),(6,2),(6,5)]

aislados :: (Ix v, Num p) => Grafo v p -> [v]
aislados g =
  [n | n <- nodos g, null (adyacentes g n) || null (incidentes g n)]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    aislados grafo1 `shouldBe` [1,2,4]

-- La verificación es
--    λ> verifica
--
--    e1
--
--    Finished in 0.0008 seconds
--    1 example, 0 failures
