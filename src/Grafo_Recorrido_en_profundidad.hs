-- Grafo_Recorrido_en_profundidad.hs
-- TAD de los grafos: Recorrido en profundidad.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 14-junio-2023
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Usando el [tipo abstracto de datos de los grafos](https://bit.ly/45cQ3Fo),
-- definir la función,
--    recorridoEnProfundidad :: (Num p, Eq p, Ix v) => v -> Grafo v p -> [v]
-- tal que (recorridoEnProfundidad i g) es el recorrido en profundidad
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
--    recorridoEnProfundidad 1 grafo1  ==  [1,2,3,6,5,4]
-- -----------------------------------------------------------

module Grafo_Recorrido_en_profundidad where
import TAD.Grafo (Grafo, Orientacion (D, ND), adyacentes,
                  creaGrafo')
import Data.Ix (Ix)
import Test.Hspec (Spec, hspec, it, shouldBe)

grafo1 :: Grafo Int Int
grafo1 = creaGrafo' D (1,6) [(1,2),(1,3),(1,4),(3,6),(5,4),(6,2),(6,5)]

-- 1ª solución
-- ===========

recorridoEnProfundidad1 :: (Num p, Eq p, Ix v) => v -> Grafo v p -> [v]
recorridoEnProfundidad1 i g = rp [i] []
  where
    rp [] vis    = vis
    rp (c:cs) vis
        | c `elem` vis = rp cs vis
        | otherwise    = rp (adyacentes g c ++ cs) (vis ++ [c])

-- Traza del cálculo de (recorridoEnProfundidad1 1 grafo1)
--    recorridoEnProfundidad1 1 grafo1
--    = rp [1]     []
--    = rp [2,3,4] [1]
--    = rp [3,4]   [1,2]
--    = rp [6,4]   [1,2,3]
--    = rp [2,5,4] [1,2,3,6]
--    = rp [5,4]   [1,2,3,6]
--    = rp [4,4]   [1,2,3,6,5]
--    = rp [4]     [1,2,3,6,5,4]
--    = rp []      [1,2,3,6,5,4]
--    = [1,2,3,6,5,4]

-- 2ª solución
-- ===========

recorridoEnProfundidad :: (Num p, Eq p, Ix v) => v -> Grafo v p -> [v]
recorridoEnProfundidad i g = reverse (rp [i] [])
  where
    rp [] vis     = vis
    rp (c:cs) vis
        | c `elem` vis = rp cs vis
        | otherwise    = rp (adyacentes g c ++ cs) (c:vis)

-- Traza del cálculo de (recorridoEnProfundidad 1 grafo1)
--    RecorridoEnProfundidad 1 grafo1
--    = reverse (rp [1]     [])
--    = reverse (rp [2,3,4] [1])
--    = reverse (rp [3,4]   [2,1])
--    = reverse (rp [6,4]   [3,2,1])
--    = reverse (rp [2,5,4] [6,3,2,1])
--    = reverse (rp [5,4]   [6,3,2,1])
--    = reverse (rp [4,4]   [5,6,3,2,1])
--    = reverse (rp [4]     [4,5,6,3,2,1])
--    = reverse (rp []      [4,5,6,3,2,1])
--    = reverse [4,5,6,3,2,1]
--    = [1,2,3,6,5,4]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    recorridoEnProfundidad1 1 grafo1 `shouldBe` [1,2,3,6,5,4]
  it "e2" $
    recorridoEnProfundidad 1 grafo1 `shouldBe` [1,2,3,6,5,4]
  it "e3" $
    recorridoEnProfundidad1 1 grafo2 `shouldBe` [1,2,6,3,5,4]
  it "e4" $
    recorridoEnProfundidad 1 grafo2 `shouldBe` [1,2,6,3,5,4]
  where
    grafo2 :: Grafo Int Int
    grafo2 = creaGrafo' ND (1,6) [(1,2),(1,3),(1,4),(3,6),(5,4),(6,2),(6,5)]

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--    e4
--
--    Finished in 0.0022 seconds
--    4 examples, 0 failures
