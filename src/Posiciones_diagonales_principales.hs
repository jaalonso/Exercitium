-- Posiciones_diagonales_principales.hs
-- Posiciones de las diagonales principales.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-febrero-2025
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las posiciones de una matriz con 3 filas y 4 columnas son
--    (1,1) (1,2) (1,3) (1,4)
--    (2,1) (2,2) (2,3) (2,4)
--    (3,1) (3,2) (3,3) (3,4)
-- Las posiciones de sus 6 diagonales principales son
--   [(3,1)]
--   [(2,1),(3,2)]
--   [(1,1),(2,2),(3,3)]
--   [(1,2),(2,3),(3,4)]
--   [(1,3),(2,4)]
--   [(1,4)]
--
-- Definir la función
--    posicionesDiagonalesPrincipales :: Int -> Int -> [[(Int, Int)]]
-- tal que (posicionesdiagonalesprincipales m n) es la lista de las
-- posiciones de las diagonales principales de una matriz con m filas y
-- n columnas. Por ejemplo,
--   λ> mapM_ print (posicionesDiagonalesPrincipales 3 4)
--   [(3,1)]
--   [(2,1),(3,2)]
--   [(1,1),(2,2),(3,3)]
--   [(1,2),(2,3),(3,4)]
--   [(1,3),(2,4)]
--   [(1,4)]
--   λ> mapM_ print (posicionesDiagonalesPrincipales 4 4)
--   [(4,1)]
--   [(3,1),(4,2)]
--   [(2,1),(3,2),(4,3)]
--   [(1,1),(2,2),(3,3),(4,4)]
--   [(1,2),(2,3),(3,4)]
--   [(1,3),(2,4)]
--   [(1,4)]
--   λ> mapM_ print (posicionesDiagonalesPrincipales 4 3)
--   [(4,1)]
--   [(3,1),(4,2)]
--   [(2,1),(3,2),(4,3)]
--   [(1,1),(2,2),(3,3)]
--   [(1,2),(2,3)]
--   [(1,3)]
-- ---------------------------------------------------------------------

module Posiciones_diagonales_principales where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

posicionesDiagonalesPrincipales1 :: Int -> Int -> [[(Int, Int)]]
posicionesDiagonalesPrincipales1 m n =
  [extension ij | ij <- iniciales]
  where iniciales = [(i,1) | i <- [m,m-1..2]] ++ [(1,j) | j <- [1..n]]
        extension (i,j) = [(i+k,j+k) | k <- [0..min (m-i) (n-j)]]

-- 2ª solución
-- ===========

posicionesDiagonalesPrincipales2 :: Int -> Int -> [[(Int, Int)]]
posicionesDiagonalesPrincipales2 m n =
  [zip [i..m] [1..n] | i <- [m,m-1..1]] ++
  [zip [1..m] [j..n] | j <- [2..n]]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> Int -> [[(Int, Int)]]) -> Spec
specG posicionesDiagonalesPrincipales = do
  it "e1" $
    posicionesDiagonalesPrincipales 3 4 `shouldBe`
      [[(3,1)],
       [(2,1),(3,2)],
       [(1,1),(2,2),(3,3)],
       [(1,2),(2,3),(3,4)],
       [(1,3),(2,4)],
       [(1,4)]]
  it "e2" $
    posicionesDiagonalesPrincipales 4 4 `shouldBe`
      [[(4,1)],
       [(3,1),(4,2)],
       [(2,1),(3,2),(4,3)],
       [(1,1),(2,2),(3,3),(4,4)],
       [(1,2),(2,3),(3,4)],
       [(1,3),(2,4)],
       [(1,4)]]
  it "e3" $
    posicionesDiagonalesPrincipales 4 3 `shouldBe`
      [[(4,1)],
       [(3,1),(4,2)],
       [(2,1),(3,2),(4,3)],
       [(1,1),(2,2),(3,3)],
       [(1,2),(2,3)],
       [(1,3)]]

spec :: Spec
spec = do
  describe "def. 1" $ specG posicionesDiagonalesPrincipales1
  describe "def. 2" $ specG posicionesDiagonalesPrincipales2

-- La verificación es
--    λ> verifica
--    6 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_posicionesDiagonalesPrincipales_equiv :: Positive Int -> Positive Int -> Bool
prop_posicionesDiagonalesPrincipales_equiv (Positive m) (Positive n) =
  posicionesDiagonalesPrincipales1 m n ==
  posicionesDiagonalesPrincipales2 m n

-- La comprobación es
--   λ> quickCheck prop_posicionesDiagonalesPrincipales_equiv
--   +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--   λ> length (posicionesDiagonalesPrincipales1 (10^7) (10^6))
--   10999999
--   (6.14 secs, 3,984,469,440 bytes)
--   λ> length (posicionesDiagonalesPrincipales2 (10^7) (10^6))
--   10999999
--   (3.07 secs, 2,840,469,440 bytes)
