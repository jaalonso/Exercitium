-- Grafo_Recorrido_en_anchura.hs
-- TAD de los grafos: Recorrido en anchura.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 15-junio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir la función,
--    recorridoEnAnchura :: (Num p, Eq p, Ix v) => v -> Grafo v p -> [v]
-- tal que (recorridoEnAnchura i g) es el recorrido en anchura
-- del grafo g desde el vértice i. Por ejemplo, en el grafo
--
--    +---> 2 <---+
--    |           |
--    |           |
--    1 --> 3 --> 6 --> 5
--    |                 |
--    |                 |
--    +---> 4 <---------+
--
-- definido por
--    grafo1 :: Grafo Int Int
--    grafo1 = creaGrafo' D (1,6) [(1,2),(1,3),(1,4),(3,6),(5,4),(6,2),(6,5)]
-- entonces
--    recorridoEnAnchura 1 grafo1  ==  [1,2,3,4,6,5]
-- -----------------------------------------------------------

module Grafo_Recorrido_en_anchura where
import TAD.Grafo (Grafo, Orientacion (D, ND), adyacentes,
                  creaGrafo')
import Data.Ix (Ix)
import Test.Hspec (Spec, hspec, it, shouldBe)

grafo1 :: Grafo Int Int
grafo1 = creaGrafo' D (1,6) [(1,2),(1,3),(1,4),(3,6),(5,4),(6,2),(6,5)]

recorridoEnAnchura :: (Num p, Eq p, Ix v) => v -> Grafo v p -> [v]
recorridoEnAnchura i g = reverse (ra [i] [])
  where
    ra [] vis    = vis
    ra (c:cs) vis
        | c `elem` vis = ra cs vis
        | otherwise    = ra (cs ++ adyacentes g c) (c:vis)

-- Traza del cálculo de (recorridoEnAnchura1 1 grafo1)
--    recorridoEnAnchura1 1 grafo1
--    = ra [1]     []
--    = ra [2,3,4] [1]
--    = ra [3,4]   [2,1]
--    = ra [4,6]   [3,2,1]
--    = ra [6]     [4,3,2,1]
--    = ra [2,5]   [6,4,3,2,1]
--    = ra [5]     [6,4,3,2,1]
--    = ra [4]     [5,6,4,3,2,1]
--    = ra []      [5,6,4,3,2,1]
--    = [1,2,3,4,6,5]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    recorridoEnAnchura 1 grafo1 `shouldBe` [1,2,3,4,6,5]
  it "e2" $
    recorridoEnAnchura 1 grafo2 `shouldBe` [1,2,3,4,6,5]
  where
    grafo2 :: Grafo Int Int
    grafo2 = creaGrafo' ND (1,6) [(1,2),(1,3),(1,4),(3,6),(5,4),(6,2),(6,5)]

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--
--    Finished in 0.0010 seconds
--    2 examples, 0 failures
