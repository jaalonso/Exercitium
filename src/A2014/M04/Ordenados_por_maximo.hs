-- Ordenados_por_maximo.hs
-- Ordenación por el máximo.
-- José A. Alonso Jiménez https://jaalonso.github.io
-- Sevilla, 22 de Abril de 2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    ordenadosPorMaximo :: Ord a => [[a]] -> [[a]]
-- tal que (ordenadosPorMaximo xss) es la lista de los elementos de xss
-- ordenada por sus máximos. Por ejemplo,
--    ghci> ordenadosPorMaximo [[3,2],[6,7,5],[1,4]]
--    [[3,2],[1,4],[6,7,5]]
--    ghci> ordenadosPorMaximo ["este","es","el","primero"]
--    ["el","primero","es","este"]
-- ---------------------------------------------------------------------

module A2014.M04.Ordenados_por_maximo where

import Data.List (sort)
import GHC.Exts (sortWith)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

-- 1ª definición
ordenadosPorMaximo1 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo1 xss =
    map snd (sort [(maximum xs,xs) | xs <- xss])

-- 2ª definición
ordenadosPorMaximo2 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo2 xss =
    [xs | (_,xs) <- sort [(maximum xs,xs) | xs <- xss]]

-- 3ª definición:
ordenadosPorMaximo3 :: Ord a => [[a]] -> [[a]]
ordenadosPorMaximo3 = sortWith maximum

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([[Int]] -> [[Int]])  -> Spec
specG ordenadosPorMaximo = do
  it "e1" $
    ordenadosPorMaximo [[3,2],[6,7,5],[1,4]] `shouldBe`
      [[3,2],[1,4],[6,7,5]]

spec :: Spec
spec = do
  describe "def. 1" $ specG ordenadosPorMaximo1
  describe "def. 2" $ specG ordenadosPorMaximo2
  describe "def. 3" $ specG ordenadosPorMaximo3

-- La verificación es
--    λ> verifica
--    3 examples, 0 failures
