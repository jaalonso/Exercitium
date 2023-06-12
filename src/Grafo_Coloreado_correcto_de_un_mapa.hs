-- Grafo_Coloreado_correcto_de_un_mapa.hs
-- TAD de los grafos: Coloreado correcto de un mapa.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 19-junio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
--  Un mapa se puede representar mediante un grafo donde los vértices
--  son las regiones del mapa y hay una arista entre dos vértices si las
--  correspondientes regiones son vecinas. Por ejemplo, el mapa siguiente
--        +----------+----------+
--        |    1     |     2    |
--        +----+-----+-----+----+
--        |    |           |    |
--        | 3  |     4     | 5  |
--        |    |           |    |
--        +----+-----+-----+----+
--        |    6     |     7    |
--        +----------+----------+
-- se pueden representar por
--    mapa :: Grafo Int Int
--    mapa = creaGrafo' ND (1,7)
--                      [(1,2),(1,3),(1,4),(2,4),(2,5),(3,4),
--                       (3,6),(4,5),(4,6),(4,7),(5,7),(6,7)]
--
-- Para colorear el mapa se dispone de 4 colores definidos por
--    data Color = A | B | C | D
--      deriving (Eq, Show)
--
-- Definir la función
--    correcta :: [(Int,Color)] -> Grafo Int Int -> Bool
-- tal que (correcta ncs m) se verifica si ncs es una coloración del
-- mapa m tal que todos las regiones vecinas tienen colores distintos.
-- Por ejemplo,
--    correcta [(1,A),(2,B),(3,B),(4,C),(5,A),(6,A),(7,B)] mapa == True
--    correcta [(1,A),(2,B),(3,A),(4,C),(5,A),(6,A),(7,B)] mapa == False
-- ---------------------------------------------------------------------

module Grafo_Coloreado_correcto_de_un_mapa where

import TAD.Grafo (Grafo, Orientacion (ND), aristas, creaGrafo')
import Test.Hspec (Spec, hspec, it, shouldBe)

mapa :: Grafo Int Int
mapa = creaGrafo' ND (1,7)
                  [(1,2),(1,3),(1,4),(2,4),(2,5),(3,4),
                   (3,6),(4,5),(4,6),(4,7),(5,7),(6,7)]

data Color = A | B | C | E
  deriving (Eq, Show)

correcta :: [(Int,Color)] -> Grafo Int Int -> Bool
correcta ncs g =
  and [color x /= color y | ((x,y),_) <- aristas g]
  where color x = head [c | (y,c) <- ncs, y == x]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    correcta [(1,A),(2,B),(3,B),(4,C),(5,A),(6,A),(7,B)] mapa `shouldBe` True
  it "e2" $
    correcta [(1,A),(2,B),(3,A),(4,C),(5,A),(6,A),(7,B)] mapa `shouldBe` False

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--
--    Finished in 0.0004 seconds
--    2 examples, 0 failures
