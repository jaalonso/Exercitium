-- Aplicaciones_alternativas.hs
-- Aplicaciones alternativas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 26-Noviembre-2014 (actualizado 17-Noviembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    alternativa :: (a -> b) -> (a -> b) -> [a] -> [b]
-- tal que (alternativa f g xs) es la lista obtenida aplicando
-- alternativamente las funciones f y g a los elementos de xs. Por
-- ejemplo,
--    alternativa (+1)  (+10) [1,2,3,4]    ==  [2,12,4,14]
--    alternativa (+10) (*10) [1,2,3,4,5]  ==  [11,20,13,40,15]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Aplicaciones_alternativas where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck.HigherOrder

-- 1ª solución
-- ===========

alternativa1 :: (a -> b) -> (a -> b) -> [a] -> [b]
alternativa1 _ _ [] = []
alternativa1 f _ [x] = [f x]
alternativa1 f g (x:y:xs) = f x : g y : alternativa1 f g xs

-- 2ª solución
-- ===========

alternativa2 :: (a -> b) -> (a -> b) -> [a] -> [b]
alternativa2 f g = aux True
  where
    aux _ [] = []
    aux True (y:ys)  = f y : aux False ys
    aux False (y:ys) = g y : aux True ys

-- 3ª solución
-- ===========

alternativa3 :: (a -> b) -> (a -> b) -> [a] -> [b]
alternativa3 _ _ []     = []
alternativa3 f g (x:xs) = f x : alternativa3 g f xs

-- 4ª solución
-- ===========

alternativa4 :: (a -> b) -> (a -> b) -> [a] -> [b]
alternativa4 f g xs =
  [h x | (h,x) <- zip (cycle [f,g]) xs]

-- 5ª solución
-- ===========

alternativa5 :: (a -> b) -> (a -> b) -> [a] -> [b]
alternativa5 f g =
  zipWith ($) (cycle [f,g])

-- 6º solución
-- ===========

alternativa6 :: (a -> b) -> (a -> b) -> [a] -> [b]
alternativa6 = ((zipWith id . cycle) .) . (. return) . (:)

-- Nota: Para pasar de la 5ª solución a la 6ª se usa la función
-- return que permite introducir un elemento en una lista. Por ejemplo,
--    > return 3 :: [Int]
--    [3]
-- Se puede formar listas de dos elementos mediante return. Por ejemplo,
--    [2,3]
--    = 2 : [3]
--    = 2 : return 3
--    = (:) 2 (return 3)
--    = ((:) 2) (return 3)
--    = (((:) 2) . return) 3
--    = ((. return) ((:) 2)) 3
--    = (((. return) . (:)) 2) 3
--    = ((. return) . (:)) 2 3
-- En general,
--    [x,y] = ((. return) . (:)) x y

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ((Int -> Int) -> (Int -> Int) -> [Int] -> [Int]) -> Spec
specG alternativa = do
  it "e1" $
    alternativa (+1)  (+10) [1,2,3,4]    `shouldBe`  [2,12,4,14]
  it "e2" $
    alternativa (+10) (*10) [1,2,3,4,5]  `shouldBe`  [11,20,13,40,15]

spec :: Spec
spec = do
  describe "def. 1" $ specG alternativa1
  describe "def. 2" $ specG alternativa2
  describe "def. 3" $ specG alternativa3
  describe "def. 4" $ specG alternativa4
  describe "def. 5" $ specG alternativa5
  describe "def. 6" $ specG alternativa6

-- La verificación es
--    λ> verifica
--    12 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_equivalencia :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
prop_equivalencia f g xs =
  all (== alternativa1 f g xs)
      [alternativa2 f g xs,
       alternativa3 f g xs,
       alternativa4 f g xs,
       alternativa5 f g xs,
       alternativa6 f g xs]

-- La comprobación es
--    λ> quickCheck' prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last (alternativa1 (+1) (+2) [1..10^7])
--    10000002
--    (2.18 secs, 2,400,602,664 bytes)
--    λ> last (alternativa2 (+1) (+2) [1..10^7])
--    10000002
--    (2.33 secs, 2,720,602,752 bytes)
--    λ> last (alternativa3 (+1) (+2) [1..10^7])
--    10000002
--    (2.04 secs, 2,640,602,640 bytes)
--    λ> last (alternativa4 (+1) (+2) [1..10^7])
--    10000002
--    (1.85 secs, 2,720,602,976 bytes)
--    λ> last (alternativa5 (+1) (+2) [1..10^7])
--    10000002
--    (0.26 secs, 1,760,602,888 bytes)
--    λ> last (alternativa6 (+1) (+2) [1..10^7])
--    10000002
--    (0.31 secs, 1,760,603,120 bytes)
