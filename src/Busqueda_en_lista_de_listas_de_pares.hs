-- Busqueda_en_lista_de_listas_de_pares.hs
-- Búsqueda en lista de listas de pares.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 8-Diciembre-2014 (actualizado 3-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    busca :: Eq a => a -> [[(a,b)]] -> [b]
-- tal que (busca x pss) es la lista de los segundos componentes de los
-- pares de la lista de listas de pares pss cuya primera componentes es
-- x. Por ejemplo,
--    busca 3 [[(3,4)],[(5,4),(3,2),(3,5)],[(4,1),(7,3)]] == [4,2,5]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Busqueda_en_lista_de_listas_de_pares where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

busca1 :: Eq a => a -> [[(a,b)]] -> [b]
busca1 x pss = [y | ps <- pss, (z,y) <- ps, z == x]

-- 2ª solución
-- ===========

busca2 :: Eq a => a -> [[(a,b)]] -> [b]
busca2 x pss = [y | (z,y) <- concat pss, z == x]

-- 3ª solución
-- ===========

busca3 :: Eq a => a -> [[(a,b)]] -> [b]
busca3 x pss = map snd (filter (\(z,_) -> z == x) (concat pss))

-- 4ª solución
-- ===========

busca4 :: Eq a => a -> [[(a,b)]] -> [b]
busca4 x = map snd . filter ((== x) . fst) . concat

-- 5ª solución
-- ===========

busca5 :: Eq a => a -> [[(a,b)]] -> [b]
busca5 x = concatMap (map snd . filter ((== x) . fst))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> [[(Int,Int)]] -> [Int]) -> Spec
specG busca = do
  it "e1" $
    busca 3 [[(3,4)],[(5,4),(3,2),(3,5)],[(4,1),(7,3)]] `shouldBe` [4,2,5]

spec :: Spec
spec = do
  describe "def. 1" $ specG busca1
  describe "def. 2" $ specG busca2
  describe "def. 3" $ specG busca3
  describe "def. 4" $ specG busca4
  describe "def. 5" $ specG busca5

-- La verificación es
--    λ> verifica
--    5 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Int -> [[(Int,Int)]] -> Bool
prop_equivalencia n xss =
  all (== busca1 n xss)
      [busca2 n xss,
       busca3 n xss,
       busca4 n xss,
       busca5 n xss]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (busca1 3 [[(i,j) | i <- [1..2000]] | j <- [1..2000]])
--    2000
--    (1.75 secs, 897,558,320 bytes)
--    λ> length (busca2 3 [[(i,j) | i <- [1..2000]] | j <- [1..2000]])
--    2000
--    (2.30 secs, 1,121,414,384 bytes)
--    λ> length (busca3 3 [[(i,j) | i <- [1..2000]] | j <- [1..2000]])
--    2000
--    (1.64 secs, 1,121,558,392 bytes)
--    λ> length (busca4 3 [[(i,j) | i <- [1..2000]] | j <- [1..2000]])
--    2000
--    (1.32 secs, 1,025,558,856 bytes)
--    λ> length (busca5 3 [[(i,j) | i <- [1..2000]] | j <- [1..2000]])
--    2000
--    (1.36 secs, 801,734,784 bytes)
