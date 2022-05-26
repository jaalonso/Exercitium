-- El_triangulo_de_Floyd.hs
-- El triángulo de Floyd.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 26-mayo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El [triángulo de Floyd](http://bit.ly/1D6ZF4q), llamado así en honor
-- a Robert Floyd, es un triángulo rectángulo formado con números
-- naturales. Para crear un triángulo de Floyd, se comienza con un 1 en
-- la esquina superior izquierda, y se continúa escribiendo la secuencia
-- de los números naturales de manera que cada línea contenga un número
-- más que la anterior. Las 5 primeras líneas del triángulo de Floyd son
--     1
--     2   3
--     4   5   6
--     7   8   9  10
--    11  12  13  14  15
--
-- Definir la función
--    trianguloFloyd :: [[Integer]]
-- tal que trianguloFloyd es el triángulo de Floyd. Por ejemplo,
--    λ> take 4 trianguloFloyd
--    [[1],
--     [2,3],
--     [4,5,6],
--     [7,8,9,10]]
--   (trianguloFloyd !! (10^5)) !! 0  ==  5000050001
--   (trianguloFloyd !! (10^6)) !! 0  ==  500000500001
--   (trianguloFloyd !! (10^7)) !! 0  ==  50000005000001
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module El_triangulo_de_Floyd where

import Data.List (genericLength)
import Test.QuickCheck (Positive (Positive), quickCheck)

-- 1ª solución
-- ===========

trianguloFloyd1 :: [[Integer]]
trianguloFloyd1 = floyd 1 [1..]
  where floyd n xs = i : floyd (n+1) r
          where (i,r) = splitAt n xs

-- 2ª solución
-- ===========

trianguloFloyd2 :: [[Integer]]
trianguloFloyd2 = iterate siguienteF [1]

-- (siguienteF xs) es la lista de los elementos de la línea xs en el
-- triángulo de Floyd. Por ejemplo,
--    siguienteF [2,3]    ==  [4,5,6]
--    siguienteF [4,5,6]  ==  [7,8,9,10]
siguienteF :: [Integer] -> [Integer]
siguienteF xs = [a..a+n]
    where a = 1 + last xs
          n = genericLength xs

-- 3ª solución
-- ===========

trianguloFloyd3 :: [[Integer]]
trianguloFloyd3 =
  [[(n*(n-1) `div` 2) + 1 .. (n*(n+1) `div` 2)] | n <- [1..]]

-- 4ª solución
-- ===========

trianguloFloyd4 :: [[Integer]]
trianguloFloyd4 =
  scanl (\(x:_) y -> [x+y..x+2*y]) [1] [1..]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_trianguloFloyd :: Positive Int -> Bool
prop_trianguloFloyd (Positive n) =
  all (== (trianguloFloyd1 !! n))
      [trianguloFloyd2 !! n,
       trianguloFloyd3 !! n,
       trianguloFloyd4 !! n]

-- La comprobación es
-- λ> quickCheck prop_trianguloFloyd
-- +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> (trianguloFloyd1 !! 5000) !! 5000
--    12507501
--    (1.47 secs, 2,505,005,752 bytes)
--    λ> (trianguloFloyd2 !! 5000) !! 5000
--    12507501
--    (0.79 secs, 2,416,259,176 bytes)
--    λ> (trianguloFloyd3 !! 5000) !! 5000
--    12507501
--    (0.00 secs, 1,809,152 bytes)
--    λ> (trianguloFloyd4 !! 5000) !! 5000
--    12507501
--    (0.01 secs, 3,517,896 bytes)
--
--    λ> (trianguloFloyd3 !! (10^7)) !! 0
--    50000005000001
--    (2.45 secs, 1,656,534,080 bytes)
--    λ> (trianguloFloyd4 !! (10^7)) !! 0
--    50000005000001
--    (10.86 secs, 5,302,760,752 bytes)
