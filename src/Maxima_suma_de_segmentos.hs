-- Maxima_suma_de_segmentos.hs
-- Máxima suma de los segmentos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 13-Enero-2015 (actualizado 30-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un segmento de una lista xs es una sublista de xs formada por
-- elementos consecutivos en la lista. El problema de la máxima suma
-- de segmentos consiste en dada una lista de números enteros calcular
-- el máximo de las sumas de todos los segmentos de la lista. Por
-- ejemplo, para la lista [-1,2,-3,5,-2,1,3,-2,-2,-3,6] la máxima suma
-- de segmentos es 7 que es la suma del segmento [5,-2,1,3] y para la
-- lista [-1,-2,-3] es 0 que es la suma de la lista vacía.
--
-- Definir la función
--    mss :: [Integer] -> Integer
-- tal que (mss xs) es la máxima suma de los segmentos de xs. Por
-- ejemplo,
--    mss [-1,2,-3,5,-2,1,3,-2,-2,-3,6]  ==  7
--    mss [-1,-2,-3]                     ==  0
--    mss [1..500]                       ==  125250
--    mss [1..1000]                      ==  500500
--    mss [-500..3]                      ==  6
--    mss [-1000..3]                     ==  6
--    mss [1..10^2]                      ==  5050
--    mss [1..10^3]                      ==  500500
--    mss [1..10^4]                      ==  50005000
--    mss [1..10^5]                      ==  5000050000
--    mss [1..10^6]                      ==  500000500000
--    mss [1..10^7]                      ==  50000005000000
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Maxima_suma_de_segmentos where

import Data.List (inits,tails)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

mss1 :: [Integer] -> Integer
mss1 = maximum . map sum . segmentos

-- (segmentos xs) es la lista de los segmentos de xs. Por ejemplo,
--    λ> segmentos "abc"
--    ["","a","ab","abc","","b","bc","","c",""]
segmentos :: [a] -> [[a]]
segmentos = concatMap inits . tails

-- 2ª solución
-- ===========

mss2 :: [Integer] -> Integer
mss2 = maximum . map sum . concatMap tails . inits

-- 3ª solución
-- ===========

mss3 :: [Integer] -> Integer
mss3 = maximum . map (maximum . scanl (+) 0) . tails

-- 4ª soución
-- ==========

mss4 :: [Integer] -> Integer
mss4  = fst . foldr (\x (b,a) -> (max (a+x) b, max 0 (a+x))) (0,0)

-- 5ª solución
-- ===========

mss5 :: [Integer] -> Integer
mss5 = maximum . scanl (\a x -> max 0 a + x) 0

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Integer] -> Integer) -> Spec
specG mss = do
  it "e1" $
    mss [-1,2,-3,5,-2,1,3,-2,-2,-3,6]  `shouldBe`  7
  it "e2" $
    mss [-1,-2,-3]                     `shouldBe`  0

spec :: Spec
spec = do
  describe "def. 1" $ specG mss1
  describe "def. 2" $ specG mss2
  describe "def. 3" $ specG mss3
  describe "def. 4" $ specG mss4
  describe "def. 5" $ specG mss5

-- La verificación es
--    λ> verifica
--    10 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: [Integer] -> Bool
prop_equivalencia xs =
  all (== mss1 xs)
      [mss2 xs,
       mss3 xs,
       mss4 xs,
       mss5 xs]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> mss1 [1..10^3]
--    500500
--    (6.30 secs, 10,101,462,472 bytes)
--    λ> mss2 [1..10^3]
--    500500
--    (1.47 secs, 2,785,948,600 bytes)
--    λ> mss3 [1..10^3]
--    500500
--    (0.05 secs, 60,972,432 bytes)
--    λ> mss4 [1..10^3]
--    500500
--    (0.01 secs, 1,162,816 bytes)
--    λ> mss5 [1..10^3]
--    500500
--    (0.01 secs, 847,912 bytes)
--
--    λ> mss3 [1..2*10^4]
--    200010000
--    (6.63 secs, 24,008,043,152 bytes)
--    λ> mss4 [1..2*10^4]
--    200010000
--    (0.05 secs, 13,207,656 bytes)
--    λ> mss5 [1..2*10^4]
--    200010000
--    (0.04 secs, 5,562,912 bytes)
--
--    λ> mss4 [1..2*10^6]
--    2000001000000
--    (3.44 secs, 1,268,405,872 bytes)
--    λ> mss5 [1..2*10^6]
--    2000001000000
--    (0.64 secs, 496,605,992 bytes)

-- ---------------------------------------------------------------------
-- § Referencia                                                       --
-- ---------------------------------------------------------------------

-- Basado en la función mss del libro de Bird "Thinking functionally
-- with Haskell" p. 127
