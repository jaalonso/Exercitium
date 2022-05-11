-- Particiones_de_enteros_positivos.hs
-- Particiones de enteros positivos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 10-mayo-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una [partición)(http://bit.ly/1KtLkNZ) de un entero positivo n es una
-- manera de escribir n como una suma de enteros positivos. Dos sumas
-- que sólo difieren en el orden de sus sumandos se consideran la misma
-- partición. Por ejemplo, 4 tiene cinco particiones: 4, 3+1, 2+2, 2+1+1
-- y 1+1+1+1.
--
-- Definir la función
--    particiones :: Int -> [[Int]]
-- tal que (particiones n) es la lista de las particiones del número
-- n. Por ejemplo,
--    particiones 4  ==  [[4],[3,1],[2,2],[2,1,1],[1,1,1,1]]
--    particiones 5  ==  [[5],[4,1],[3,2],[3,1,1],[2,2,1],[2,1,1,1],[1,1,1,1,1]]
--    length (particiones 50)  ==  204226
-- ---------------------------------------------------------------------

module Particiones_de_enteros_positivos where

import Test.QuickCheck

-- 1ª solución
-- ===========

particiones1 :: Int -> [[Int]]
particiones1 0 = [[]]
particiones1 n = [x:y | x <- [n,n-1..1],
                        y <- particiones1 (n-x),
                        [x] >= take 1 y]

-- 2ª solución
-- ===========

particiones2 :: Int -> [[Int]]
particiones2 n = aux !! n
  where
    aux = [] : map particiones [1..]
    particiones m = [m] : [x:p | x <- [m,m-1..1],
                                 p <- aux !! (m-x),
                                 x >= head p]

-- 3ª solución
-- ===========

particiones3 :: Int -> [[Int]]
particiones3 n = aux n n
  where aux 0 _ = [[]]
        aux n' m = concat [map (i:) (aux (n'-i) i)
                          | i <- [n',n'-1..1], i <= m]

-- 4ª solución
-- ===========

particiones4 :: Int -> [[Int]]
particiones4 n = aux n n
  where aux 0 _ = [[]]
        aux n' m = concat [map (i:) (aux (n'-i) i)
                          | i <- [k,k-1..1]]
          where k = min m n'

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_particiones :: Positive Int -> Bool
prop_particiones (Positive n) =
  all (== particiones1 n)
      [ particiones2 n
      , particiones3 n
      , particiones4 n
      ]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=20}) prop_particiones
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia                                        --
-- =========================

-- La comparación es
--    λ> length (particiones1 23)
--    1255
--    (12.50 secs, 6,614,487,992 bytes)
--    λ> length (particiones2 23)
--    1255
--    (0.04 secs, 3,071,104 bytes)
--    λ> length (particiones3 23)
--    1255
--    (0.02 secs, 9,163,544 bytes)
--    λ> length (particiones4 23)
--    1255
--    (0.01 secs, 7,149,512 bytes)
--
--    λ> length (particiones2 50)
--    204226
--    (2.50 secs, 758,729,104 bytes)
--    λ> length (particiones3 50)
--    204226
--    (4.26 secs, 2,359,121,096 bytes)
--    λ> length (particiones4 50)
--    204226
--    (2.67 secs, 1,598,588,040 bytes)
