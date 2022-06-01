-- Descomposiciones_con_sumandos_1_o_2.hs
-- Descomposiciones con sumandos 1 o 2.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 3-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la funciones
--    sumas  :: Int -> [[Int]]
--    nSumas :: Int -> Integer
-- tales que 
-- + (sumas n) es la lista de las descomposiciones de n como sumas
--    cuyos sumandos son 1 ó 2. Por ejemplo,
--       sumas 1            ==  [[1]]
--       sumas 2            ==  [[1,1],[2]]
--       sumas 3            ==  [[1,1,1],[1,2],[2,1]]
--       sumas 4            ==  [[1,1,1,1],[1,1,2],[1,2,1],[2,1,1],[2,2]]
--       length (sumas 26)  ==  196418
--       length (sumas 33)  ==  5702887
-- + (nSumas n) es el número de descomposiciones de n como sumas cuyos
--   sumandos son 1 ó 2. Por ejemplo, 
--       nSumas 4                      ==  5
--       nSumas 123                    ==  36726740705505779255899443
--       length (show (nSumas 123456)) ==  25801
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Descomposiciones_con_sumandos_1_o_2 where

import Data.List  (genericIndex, genericLength)
import Data.Array ((!), array)
import Test.QuickCheck (Positive(Positive), quickCheckWith)

-- 1ª solución de sumas
-- ====================

sumas1 :: Int -> [[Int]]
sumas1 0 = [[]]
sumas1 1 = [[1]]
sumas1 n = [1:xs | xs <- sumas1 (n-1)] ++ [2:xs | xs <- sumas1 (n-2)]

-- 2ª solución de sumas
-- ====================

sumas2 :: Int -> [[Int]]
sumas2 0 = [[]]
sumas2 1 = [[1]]
sumas2 n = map (1:) (sumas2 (n-1)) ++ map (2:) (sumas2 (n-2))

-- 3ª solución de sumas
-- ====================

sumas3 :: Int -> [[Int]]
sumas3 n = v ! n
  where v = array (0,n) [(i, f i) | i <- [0..n]]
        f 0 = [[]]
        f 1 = [[1]]
        f k = map (1:) (v!(k-1)) ++ map (2:) (v!(k-2))
 
-- 4ª solución de sumas
-- ====================

sumas4 :: Int -> [[Int]]
sumas4 n = sucSumas !! n

-- sucSumas es la sucesión cuyo n-ésimo elemento es la lista de las
-- descomposiciones de n como sumas cuyos sumandos son 1 ó 2. Por
-- ejemplo,
--    λ> take 4 sucSumas
--    [[[]],[[1]],[[1,1],[2]],[[1,1,1],[1,2],[2,1]]]
--    λ> mapM_ print (take 5 sucSumas)
--    [[]]
--    [[1]]
--    [[1,1],[2]]
--    [[1,1,1],[1,2],[2,1]]
--    [[1,1,1,1],[1,1,2],[1,2,1],[2,1,1],[2,2]]
sucSumas :: [[[Int]]]
sucSumas = [[]] : [[1]] : zipWith f (tail sucSumas) sucSumas
  where f xs ys = map (1:) xs ++ map (2:) ys

-- Comprobación de equivalencia de sumas
-- =====================================

-- La propiedad es
prop_sumas :: Positive Int -> Bool
prop_sumas (Positive n) =
  all (== sumas1 n)
      [sumas2 n,
       sumas3 n,
       sumas4 n]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=20}) prop_sumas
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de sumas
-- ==================================

-- La comparación es
--    λ> length (sumas1 28)
--    514229
--    (2.79 secs, 1,739,784,512 bytes)
--    λ> length (sumas2 28)
--    514229
--    (1.33 secs, 1,512,291,248 bytes)
--    λ> length (sumas3 28)
--    514229
--    (0.20 secs, 165,215,800 bytes)
--    λ> length (sumas4 28)
--    514229
--    (0.17 secs, 165,201,592 bytes)
--
--    λ> length (sumas3 33)
--    5702887
--    (2.16 secs, 1,830,761,864 bytes)
--    λ> length (sumas4 33)
--    5702887
--    (1.44 secs, 1,830,749,832 bytes)

-- Definición de sumas
-- ===================

-- La cuarta solución es más eficiente y es la que usaremos en lo
-- sucesivo:
sumas :: Int -> [[Int]]
sumas = sumas4

-- 1ª solución de nSumas
-- =====================

nSumas1 :: Int -> Integer
nSumas1 = genericLength . sumas2

-- 2ª solución de nSumas
-- =====================

nSumas2 :: Int -> Integer
nSumas2 0 = 1
nSumas2 1 = 1
nSumas2 n = nSumas2 (n-1) + nSumas2 (n-2)

-- 3ª solución de nSumas
-- =====================

nSumas3 :: Int -> Integer
nSumas3 n = v ! n
  where v = array (0,n) [(i,f i) | i <- [0..n]]
        f 0 = 1
        f 1 = 1
        f k = v ! (k-1) + v ! (k-2)

-- 4ª solución de nSumas
-- =====================

nSumas4 :: Int -> Integer
nSumas4 n = aux `genericIndex` n
  where aux = 1 : 1 : zipWith (+) aux (tail aux) 

-- Comprobación de equivalencia de nSumas
-- ======================================

-- La propiedad es
prop_nSumas :: Positive Int -> Bool
prop_nSumas (Positive n) =
  all (== nSumas1 n)
      [nSumas2 n,
       nSumas3 n,
       nSumas4 n]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=20}) prop_nSumas
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia de nSumas
-- ===================================

-- La comparación es
--    λ> nSumas1 33
--    5702887
--    (17.32 secs, 23,140,562,600 bytes)
--    λ> nSumas2 33
--    5702887
--    (3.48 secs, 1,870,676,904 bytes)
--    λ> nSumas3 33
--    5702887
--    (0.00 secs, 152,960 bytes)
--    λ> nSumas4 33
--    5702887
--    (0.00 secs, 139,456 bytes)
--    
--    λ> length (show (nSumas3 (2*10^5)))
--    41798
--    (1.41 secs, 1,895,295,528 bytes)
--    λ> length (show (nSumas4 (2*10^5)))
--    41798
--    (2.39 secs, 1,834,998,800 bytes)

-- Nota. El valor de (nSumas n) es el n-ésimo término de la sucesión de
-- Fibonacci 1, 1, 2, 3, 5, 8, ...
