-- Grafo_Recorridos_en_un_grafo_completo.hs
-- TAD de los grafos: Recorridos en un grafo completo.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 12-junio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    recorridos :: [a] -> [[a]]
-- tal que (recorridos xs) es la lista de todos los posibles recorridos
-- por el grafo cuyo conjunto de vértices es xs y cada vértice se
-- encuentra conectado con todos los otros y los recorridos pasan por
-- todos los vértices una vez y terminan en el vértice inicial. Por
-- ejemplo,
--    λ> recorridos [2,5,3]
--    [[2,5,3,2],[5,2,3,5],[3,5,2,3],[5,3,2,5],[3,2,5,3],[2,3,5,2]]
-- Indicación: No importa el orden de los recorridos en la lista.
-- ---------------------------------------------------------------------

module Grafo_Recorridos_en_un_grafo_completo where

import Data.List (permutations)
import Test.Hspec (Spec, hspec, it, shouldBe)

recorridos :: [a] -> [[a]]
recorridos xs = [(y:ys) ++ [y] | y:ys <- permutations xs]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    recorridos [2 :: Int,5,3] `shouldBe`
    [[2,5,3,2],[5,2,3,5],[3,5,2,3],[5,3,2,5],[3,2,5,3],[2,3,5,2]]

-- La verificación es
--    λ> verifica
--
--    e1
--
--    Finished in 0.0007 seconds
--    1 example, 0 failures
