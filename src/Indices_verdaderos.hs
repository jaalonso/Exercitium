-- Indices_verdaderos.hs
-- Índices de valores verdaderos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 12-abril-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    indicesVerdaderos :: [Int] -> [Bool]
-- tal que (indicesVerdaderos xs) es la lista infinita de booleanos tal
-- que sólo son verdaderos los elementos cuyos índices pertenecen a la
-- lista estrictamente creciente xs. Por ejemplo,
--    λ> take 6 (indicesVerdaderos [1,4])
--    [False,True,False,False,True,False]
--    λ> take 6 (indicesVerdaderos [0,2..])
--    [True,False,True,False,True,False]
--    λ> take 3 (indicesVerdaderos [])
--    [False,False,False]
--    λ> take 6 (indicesVerdaderos [1..])
--    [False,True,True,True,True,True]
--    λ> last (take (8*10^7) (indicesVerdaderos [0,5..]))
--    False
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Indices_verdaderos where

import Data.List.Ordered (member)
import Test.QuickCheck

-- 1ª solución
-- ===========

indicesVerdaderos1 :: [Int] -> [Bool]
indicesVerdaderos1 []     = repeat False
indicesVerdaderos1 (x:ys) =
  replicate x False ++ [True] ++ indicesVerdaderos1 [y-x-1 | y <- ys]

-- 2ª solución
-- ===========

indicesVerdaderos2 :: [Int] -> [Bool]
indicesVerdaderos2 = aux 0
  where aux _ []     = repeat False
        aux n (x:xs) | x == n    = True  : aux (n+1) xs
                     | otherwise = False : aux (n+1) (x:xs)

-- 3ª solución
-- ===========

indicesVerdaderos3 :: [Int] -> [Bool]
indicesVerdaderos3 = aux [0..]
  where aux _      []                 = repeat False
        aux (i:is) (x:xs) | i == x    = True  : aux is xs
                          | otherwise = False : aux is (x:xs)

-- 4ª solución
-- ===========

indicesVerdaderos4 :: [Int] -> [Bool]
indicesVerdaderos4 xs = [pertenece x xs | x <- [0..]]

-- (pertenece x ys) se verifica si x pertenece a la lista estrictamente
-- creciente (posiblemente infinita) ys. Por ejemplo,
--    pertenece 9 [1,3..]  ==  True
--    pertenece 6 [1,3..]  ==  False
pertenece :: Int -> [Int] -> Bool
pertenece x ys = x `elem` takeWhile (<=x) ys

-- 5ª solución
-- ===========

indicesVerdaderos5 :: [Int] -> [Bool]
indicesVerdaderos5 xs = map (`pertenece2` xs) [0..]

pertenece2 :: Int -> [Int] -> Bool
pertenece2 x = aux
  where aux [] = False
        aux (y:ys) = case compare x y of
                       LT -> False
                       EQ -> True
                       GT -> aux ys

-- 6ª solución
-- ===========

indicesVerdaderos6 :: [Int] -> [Bool]
indicesVerdaderos6 xs = map (`member` xs) [0..]

-- Comprobación de equivalencia
-- ============================

-- ListaCreciente es un tipo de dato para generar lista de enteros
-- crecientes arbitrarias.
newtype ListaCreciente = LC [Int]
  deriving Show

-- listaCrecienteArbitraria es un generador de lista de enteros
-- crecientes arbitrarias. Por ejemplo,
--    λ> sample listaCrecienteArbitraria
--    LC []
--    LC [2,5]
--    LC [4,8]
--    LC [6,13]
--    LC [7,15,20,28,33]
--    LC [11,15,20,29,35,40]
--    LC [5,17,25,36,42,50,52,64]
--    LC [9,16,31,33,46,59,74,83,85,89,104,113,118]
--    LC [9,22,29,35,37,49,53,62,68,77,83,100]
--    LC []
--    LC [3,22,25,34,36,51,72,75,89]
listaCrecienteArbitraria :: Gen ListaCreciente
listaCrecienteArbitraria =
  LC . listaCreciente <$> arbitrary

-- (listaCreciente xs) es la lista creciente correspondiente a xs. Por ejemplo,
--    listaCreciente [-1,3,-4,3,0]   ==  [2,6,11,15,16]
listaCreciente :: [Int] -> [Int]
listaCreciente xs =
  scanl1 (+) (map (succ . abs) xs)

-- ListaCreciente está contenida en Arbitrary
instance Arbitrary ListaCreciente where
  arbitrary = listaCrecienteArbitraria

-- La propiedad es
prop_indicesVerdaderos :: ListaCreciente -> Bool
prop_indicesVerdaderos (LC xs) =
  all (== take n (indicesVerdaderos1 xs))
      [take n (f xs) | f <-[indicesVerdaderos2,
                            indicesVerdaderos3,
                            indicesVerdaderos4,
                            indicesVerdaderos5,
                            indicesVerdaderos6]]
  where n = length xs

-- La comprobación es
--    λ> quickCheck prop_indicesVerdaderos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last (take (2*10^4) (indicesVerdaderos1 [0,5..]))
--    False
--    (2.69 secs, 2,611,031,544 bytes)
--    λ> last (take (2*10^4) (indicesVerdaderos2 [0,5..]))
--    False
--    (0.03 secs, 10,228,880 bytes)
--
--    λ> last (take (4*10^6) (indicesVerdaderos2 [0,5..]))
--    False
--    (2.37 secs, 1,946,100,856 bytes)
--    λ> last (take (4*10^6) (indicesVerdaderos3 [0,5..]))
--    False
--    (1.54 secs, 1,434,100,984 bytes)
--
--    λ> last (take (6*10^6) (indicesVerdaderos3 [0,5..]))
--    False
--    (2.30 secs, 2,150,900,984 bytes)
--    λ> last (take (6*10^6) (indicesVerdaderos4 [0,5..]))
--    False
--    (1.55 secs, 1,651,701,184 bytes)
--    λ> last (take (6*10^6) (indicesVerdaderos5 [0,5..]))
--    False
--    (0.58 secs, 1,584,514,304 bytes)
--
--    λ> last (take (3*10^7) (indicesVerdaderos5 [0,5..]))
--    False
--    (2.74 secs, 7,920,514,360 bytes)
--    λ> last (take (3*10^7) (indicesVerdaderos6 [0,5..]))
--    False
--    (0.82 secs, 6,960,514,136 bytes)

--    λ> last (take (2*10^4) (indicesVerdaderos1 [0,5..]))
--    False
--    (2.69 secs, 2,611,031,544 bytes)
--    λ> last (take (2*10^4) (indicesVerdaderos6 [0,5..]))
--    False
--    (0.01 secs, 5,154,040 bytes)
