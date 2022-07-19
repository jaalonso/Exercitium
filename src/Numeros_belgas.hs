-- Numeros_belgas.hs
-- Números belgas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 26-julio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Un número n es **k-belga** si la sucesión cuyo primer elemento es k y 
-- cuyos elementos se obtienen sumando reiteradamente las cifras de n
-- contiene a n. 
-- 
-- El 18 es 0-belga, porque a partir del 0 vamos a ir sumando
-- sucesivamente 1, 8, 1, 8, ... hasta llegar o sobrepasar el 18: 0, 1,
-- 9, 10, 18, ... Como se alcanza el 18, resulta que el 18 es 0-belga. 
-- 
-- El 19 no es 1-belga, porque a partir del 1 vamos a ir sumando
-- sucesivamente 1, 9, 1, 9, ... hasta llegar o sobrepasar el 18: 0, 1,
-- 10, 11, 20, 21, ... Como no se alcanza el 19, resulta que el 19 no es
-- 1-belga. 
--
-- Definir la función 
--    esBelga :: Int -> Int -> Bool
-- tal que (esBelga k n)  se verifica si n es k-belga. Por ejemplo,
--    esBelga 0 18                              ==  True
--    esBelga 1 19                              ==  False
--    esBelga 0 2016                            ==  True
--    [x | x <- [0..30], esBelga 7 x]           ==  [7,10,11,21,27,29]
--    [x | x <- [0..30], esBelga 10 x]          ==  [10,11,20,21,22,24,26]
--    length [n | n <- [1..10^6], esBelga 0 n]  ==  272049
--
-- Comprobar con QuickCheck que para todo número entero positivo n, si
-- k es el resto de n entre la suma de los dígitos de n, entonces n es
-- k-belga.
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Numeros_belgas where

import Data.Char (digitToInt)
import Test.QuickCheck (Positive (Positive), quickCheck)

-- 1ª solución
-- ===========

esBelga1 :: Int -> Int -> Bool
esBelga1 k n =
  n == head (dropWhile (<n) (scanl (+) k (cycle (digitos n))))

digitos :: Int -> [Int]
digitos n = map digitToInt (show n)

-- 2ª solución
-- ===========

esBelga2 :: Int -> Int -> Bool
esBelga2 k n =
  k <= n && n == head (dropWhile (<n) (scanl (+) (k + q * s) ds))
  where ds = digitos n
        s  = sum ds
        q  = (n - k) `div` s

-- Equivalencia
-- ============

-- La propiedad es
prop_esBelga :: Positive Int -> Positive Int -> Bool
prop_esBelga (Positive k) (Positive n) = 
  esBelga1 k n == esBelga2 k n

-- La comprobación es
--    λ> quickCheck prop_esBelga
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length [n | n <- [1..2*10^4], esBelga1 0 n]
--    6521
--    (6.27 secs, 6,508,102,192 bytes)
--    λ> length [n | n <- [1..2*10^4], esBelga2 0 n]
--    6521
--    (0.07 secs, 46,741,144 bytes)

-- Verificación de la propiedad
-- ============================

-- La propiedad es
prop_Belga :: Positive Int -> Bool
prop_Belga (Positive n) = 
  esBelga2 k n
  where k = n `mod` sum (digitos n)

-- La comprobación es
--    λ> quickCheck prop_Belga
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- Basado en el artículo "Números belgas" http://bit.ly/1n49fPh del blog
-- "Números y hoja de cálculo" http://hojaynumeros.blogspot.com.es de
-- Antonio Roldán Martínez http://bit.ly/1nrlV3l
