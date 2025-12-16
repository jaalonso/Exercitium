-- Elementos_mas_frecuentes.hs
-- Elementos más frecuentes.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 26-Diciembre-2014 (actualizado 16-Diciembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    masFrecuentes :: Ord a => Int -> [a] -> [(Int,a)]
-- tal que (masFrecuentes n xs) es la lista de los pares formados por
-- los elementos de xs que aparecen más veces junto con el número de
-- veces que aparecen. Por ejemplo,
--    λ> masFrecuentes 2 "trianera"
--    [(2,'r'),(2,'a')]
--    λ> masFrecuentes 2 "interdisciplinariedad"
--    [(5,'i'),(3,'d')]
--    λ> masFrecuentes 3 (show (product [1..10000]))
--    [(5803,'0'),(3416,'2'),(3341,'4')]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Elementos_mas_frecuentes where

import Data.List (group, sort)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

masFrecuentes1 :: Ord a => Int -> [a] -> [(Int,a)]
masFrecuentes1 n xs =
  take n (reverse (sort [(length ys,head ys) | ys <- group (sort xs)]))

-- 2ª solución
-- ===========

masFrecuentes2 :: Ord a => Int -> [a] -> [(Int,a)]
masFrecuentes2 n = take n . reverse . sort . cuenta . group . sort
  where cuenta xss = [(length xs,head xs) | xs <- xss]

-- 3ª solución
-- ===========

masFrecuentes3 :: Ord a => Int -> [a] -> [(Int,a)]
masFrecuentes3 i =
  take i . reverse . sort . (zip . map length <*> map head) . group . sort

-- En la definición anterior se usa el operador (<*>). Su significado se
-- explica con el siguiente ejemplo,
--    (zip <*> tail) [2,3,5]
--    = zip [2,3,5] (tail [2,3,5])
--    = zip [2,3,5] [3,5]
--    = [(2,3),(3,5)]
--
-- En general,
--    (f <*> g) xs
-- es equivalente a
--    f xs (g xs)
--
-- El significado de (zip . map length <*> map head) se puede apreciar
-- con el siguiente cálculo
--    (zip . map length <*> map head) ["aa","bbb"]
--    = ((zip . map length) <*> (map head)) ["aa","bbb"]
--    = (zip . map length) ["aa","bbb"] (map head ["aa","bbb"])
--    = (zip . map length) ["aa","bbb"] "ab"
--    = zip (map length ["aa","bbb"]) "ab"
--    = zip [2,3] "ab"
--    = [(2,'a'),(3,'b')]

-- 4ª solución
-- ===========

masFrecuentes4 :: Ord a => Int -> [a] -> [(Int,a)]
masFrecuentes4 i =
  take i . reverse . sort . (map $ (,) <$> length <*> head) . group . sort

-- En la definición de anterior se ha usado el operador (<$>). Su
-- significado se explica con el siguiente ejemplo,
--    ((+) <$> length) "abc" 4
--    = (+) (length "abc") 4
--    = (+) 3 4
--    = 3 + 4
--    = 7
--
-- También se ha usado el operador (,) para construir pares. Por
-- ejemplo,
--    (,) 3 4 = (3,4)
--
-- El significado de (map $ (,) <$> length <*> head) se puede apreciar
-- con el siguiente cálculo
--    (map $ (,) <$> length <*> head) ["aa","bbb"]
--    = [((,) <$> length <*> head) "aa", ((,) <$> length <*> head) "bbb"]
--    = [(((,) <$> length) <*> head) "aa", (((,) <$> length) <*> head) "bbb"]
--    = [((,) <$> length) "aa" (head "aa"), ((,) <$> length) "bbb" (head "bbb")]
--    = [((,) <$> length) "aa" 'a', ((,) <$> length) "bbb" 'b']
--    = [(,) (length "aa") 'a', (,) (length "bbb") 'b']
--    = [(,) 2 'a', (,) 3 'b']
--    = [(2,'a'),(3,'b')]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> String -> [(Int,Char)]) -> Spec
specG masFrecuentes = do
  it "e1" $
    masFrecuentes 2 "trianera" == [(2,'r'),(2,'a')]
  it "e2" $
    masFrecuentes 2 "interdisciplinariedad" == [(5,'i'),(3,'d')]

spec :: Spec
spec = do
  describe "def. 1" $ specG masFrecuentes1
  describe "def. 2" $ specG masFrecuentes2
  describe "def. 3" $ specG masFrecuentes3
  describe "def. 4" $ specG masFrecuentes4

-- La verificación es
--    λ> verifica
--    8 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: Int -> [Int] -> Bool
prop_equivalencia n xs =
  all (== masFrecuentes1 n xs)
      [masFrecuentes2 n xs,
       masFrecuentes3 n xs,
       masFrecuentes4 n xs]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> masFrecuentes1 3 (show (product [1..100000]))
--    [(68620,'0'),(43470,'7'),(43275,'2')]
--    (2.00 secs, 9,693,045,704 bytes)
--    λ> masFrecuentes2 3 (show (product [1..100000]))
--    [(68620,'0'),(43470,'7'),(43275,'2')]
--    (1.98 secs, 9,693,046,408 bytes)
--    λ> masFrecuentes3 3 (show (product [1..100000]))
--    [(68620,'0'),(43470,'7'),(43275,'2')]
--    (1.99 secs, 9,693,047,424 bytes)
--    λ> masFrecuentes4 3 (show (product [1..100000]))
--    [(68620,'0'),(43470,'7'),(43275,'2')]
--    (2.02 secs, 9,693,046,808 bytes)

-- Referencia
-- ==========

-- Basado en la función commonWords del libro de Bird "Thinking
-- functionally with Haskell" p. 3.
