-- Numeros_naturales_separados_por_ceros.hs
-- Números naturales separados por ceros.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 26-Enero-2015 (actualizado 14-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la sucesión
--    naturalesEntreCero :: [Int]
-- cuyos elementos son los números naturales separados por 0. Por
-- ejemplo,
--    λ> take 25 naturalesEntreCero
--    [0,0,1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9,0,10,0,11,0,12]
--
-- Comprobar con QuickCheck que el n-ésimo término de la sucesión es
-- n*(1+(-1)^n)/4.
--
-- Nota. En la comprobación usar
--    quickCheckWith (stdArgs {maxSize=7}) prop_naturalesEntreCero
-- ---------------------------------------------------------------------

module Numeros_naturales_separados_por_ceros where

import Data.List (intersperse, transpose)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

naturalesEntreCero1 :: [Int]
naturalesEntreCero1 = concat [[n,0] | n <- [0..]]

-- 2ª solución
-- ===========

naturalesEntreCero2 :: [Int]
naturalesEntreCero2 = concatMap (\n -> [n, 0]) [0..]

-- 3ª solución
-- ===========

naturalesEntreCero3 :: [Int]
naturalesEntreCero3 = intersperse 0 [0..]

-- 4ª solución
-- ===========

naturalesEntreCero4 :: [Int]
naturalesEntreCero4 = intercala [0..] (repeat 0)

-- (intercala xs ys) es la lista obtenida intercalando los elementos de
-- xs e ys. Por ejemplo.
--    λ> take 10 (intercala [1,3..] [2,4..])
--    [1,2,3,4,5,6,7,8,9,10]
intercala :: [a] -> [a] -> [a]
intercala (x:xs) ys = x : intercala ys xs
intercala [] ys     = ys

-- 5ª solución
-- ===========

naturalesEntreCero5 :: [Int]
naturalesEntreCero5 = concat (transpose [[0..], repeat 0])

-- 6ª solución
-- ===========

naturalesEntreCero6 :: [Int]
naturalesEntreCero6 = aux 0
  where aux n = n : 0 : aux (n + 1)

-- 7ª solución
-- ===========

naturalesEntreCero7 :: [Int]
naturalesEntreCero7 = [0..] >>= (:[0])

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: [Int] -> Spec
specG naturalesEntreCero = do
  it "e1" $
    take 25 naturalesEntreCero `shouldBe`
    [0,0,1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9,0,10,0,11,0,12]

spec :: Spec
spec = do
  describe "def. 1" $ specG naturalesEntreCero1
  describe "def. 2" $ specG naturalesEntreCero2
  describe "def. 3" $ specG naturalesEntreCero3
  describe "def. 4" $ specG naturalesEntreCero4
  describe "def. 5" $ specG naturalesEntreCero5
  describe "def. 6" $ specG naturalesEntreCero6
  describe "def. 7" $ specG naturalesEntreCero7

-- La verificación es
--    λ> verifica
--    7 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: NonNegative Int -> Bool
prop_equivalencia (NonNegative n) =
  all (== naturalesEntreCero1 !! n)
      [naturalesEntreCero2 !! n,
       naturalesEntreCero3 !! n,
       naturalesEntreCero4 !! n,
       naturalesEntreCero5 !! n,
       naturalesEntreCero6 !! n,
       naturalesEntreCero7 !! n]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> naturalesEntreCero1 !! (10^7)
--    5000000
--    (2.37 secs, 1,840,596,888 bytes)
--    λ> naturalesEntreCero2 !! (10^7)
--    5000000
--    (1.61 secs, 1,480,597,752 bytes)
--    λ> naturalesEntreCero3 !! (10^7)
--    5000000
--    (0.44 secs, 760,599,480 bytes)
--    λ> naturalesEntreCero4 !! (10^7)
--    5000000
--    (2.04 secs, 1,640,597,368 bytes)
--    λ> naturalesEntreCero5 !! (10^7)
--    5000000
--    (1.23 secs, 2,560,595,160 bytes)
--    λ> naturalesEntreCero6 !! (10^7)
--    5000000
--    (3.02 secs, 1,848,795,416 bytes)
--    λ> naturalesEntreCero7 !! (10^7)
--    5000000
--    (0.45 secs, 1,200,598,424 bytes)
--
--    λ> naturalesEntreCero3 !! (2*10^7)
--    10000000
--    (1.54 secs, 1,520,602,320 bytes)
--    λ> naturalesEntreCero7 !! (2*10^7)
--    10000000
--    (2.58 secs, 2,400,602,488 bytes)

-- Propiedad
-- =========

-- La propiedad es
prop_naturalesEntreCero :: NonNegative Int -> Bool
prop_naturalesEntreCero (NonNegative n) =
  naturalesEntreCero1 !! n == n*(1+(-1)^n) `div` 4

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=7}) prop_naturalesEntreCero
--    +++ OK, passed 100 tests.
