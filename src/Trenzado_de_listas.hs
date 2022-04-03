-- Trenzado_de_listas.hs
-- Trenzado de listas.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-abril-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    trenza :: [a] -> [a] -> [a]
-- tal que (trenza xs ys) es la lista obtenida intercalando los
-- elementos de xs e ys. Por ejemplo,
--    trenza [5,1] [2,7,4]             ==  [5,2,1,7]
--    trenza [5,1,7] [2..]             ==  [5,2,1,3,7,4]
--    trenza [2..] [5,1,7]             ==  [2,5,3,1,4,7]
--    take 8 (trenza [2,4..] [1,5..])  ==  [2,1,4,5,6,9,8,13]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Trenzado_de_listas where

import Test.QuickCheck (quickCheck)

-- 1ª solución
-- ===========

trenza1 :: [a] -> [a] -> [a]
trenza1 []     _      = []
trenza1 _      []     = []
trenza1 (x:xs) (y:ys) = x : y : trenza1 xs ys

-- 2ª solución
-- ===========

trenza2 :: [a] -> [a] -> [a]
trenza2 (x:xs) (y:ys) = x : y : trenza2 xs ys
trenza2 _      _      = []

-- 3ª solución
-- ===========

trenza3 :: [a] -> [a] -> [a]
trenza3 xs ys = concat [[x,y] | (x,y) <- zip xs ys]

-- 4ª solución
-- ===========

trenza4 :: [a] -> [a] -> [a]
trenza4 xs ys = concat (zipWith par xs ys)

par :: a -> a -> [a]
par x y = [x,y]

-- 5ª solución
-- ===========

-- Explicación de eliminación de argumentos en composiciones con varios
-- argumentos:

f :: Int -> Int
f x = x + 1

g :: Int -> Int -> Int
g x y = x + y

h1, h2, h3, h4, h5, h6, h7 :: Int -> Int -> Int
h1 x y  = f (g x y)
h2 x y  = f ((g x) y)
h3 x y  = (f . (g x)) y
h4 x    = f . (g x)
h5 x    = (f .) (g x)
h6 x    = ((f .) . g) x
h7      = (f .) . g

prop_composicion :: Int -> Int -> Bool
prop_composicion x y =
  all (== h1 x y)
      [p x y | p <- [h2, h3, h4, h5, h6, h7]]

-- λ> quickCheck prop_composicion
-- +++ OK, passed 100 tests.

-- En general,
--    f . g             --> \x -> f (g x)
--    (f .) . g         --> \x y -> f (g x y)
--    ((f .) .) . g     --> \x y z -> f (g x y z)
--    (((f .) .) .) . g --> \w x y z -> f (g w x y z)

trenza5 :: [a] -> [a] -> [a]
trenza5 = (concat .) . zipWith par

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_trenza :: [Int] -> [Int] -> Bool
prop_trenza xs ys =
  all (== trenza1 xs ys)
      [trenza2 xs ys,
       trenza3 xs ys,
       trenza4 xs ys,
       trenza5 xs ys]

-- La comprobación es
--    λ> quickCheck prop_trenza
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last (trenza1 [1,1..] [1..4*10^6])
--    4000000
--    (2.33 secs, 1,472,494,952 bytes)
--    λ> last (trenza2 [1,1..] [1..4*10^6])
--    4000000
--    (2.24 secs, 1,376,494,928 bytes)
--    λ> last (trenza3 [1,1..] [1..4*10^6])
--    4000000
--    (1.33 secs, 1,888,495,048 bytes)
--    λ> last (trenza4 [1,1..] [1..4*10^6])
--    4000000
--    (0.76 secs, 1,696,494,968 bytes)
--    λ> last (trenza5 [1,1..] [1..4*10^6])
--    4000000
--    (0.76 secs, 1,696,495,064 bytes)
