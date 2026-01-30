-- Particiones_de_enteros_positivos.hs
-- Particiones de enteros positivos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 05-Febrero-2015 (actualizado 28-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Una partición de un entero positivo n es una manera de escribir n
-- como una suma de enteros positivos. Dos sumas que sólo difieren en el
-- orden de sus sumandos se consideran la misma partición. Por ejemplo,
-- 4 tiene cinco particiones: 4, 3+1, 2+2, 2+1+1 y 1+1+1+1.
--
-- Definir la función
--    particiones :: Int -> [[Int]]
-- tal que (particiones n) es la lista de las particiones del número
-- n. Por ejemplo,
--    particiones 4  ==  [[4],[3,1],[2,2],[2,1,1],[1,1,1,1]]
--    particiones 5  ==  [[5],[4,1],[3,2],[3,1,1],[2,2,1],[2,1,1,1],[1,1,1,1,1]]
--    length (particiones 70)  ==  4087968
-- ---------------------------------------------------------------------

module Particiones_de_enteros_positivos where

import Data.Array ((!), listArray)
import Math.Combinat.Partitions (partitions, fromPartition)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución: Recursiva básica  (generar y filtrar)
-- ==================================================

particiones1 :: Int -> [[Int]]
particiones1 0 = [[]]
particiones1 n = [x:y | x <- [n,n-1..1],
                        y <- particiones1 (n-x),
                        [x] >= take 1 y]

-- 2ª solución: Recursiva con poda
-- ===============================

particiones2 :: Int -> [[Int]]
particiones2 n = aux n n
  where aux 0 _ = [[]]
        aux n' m = concat [map (i:) (aux (n'-i) i)
                          | i <- [k,k-1..1]]
          where k = min m n'

-- 3ª solución: Notación monádica
-- ==============================

particiones3 :: Int -> [[Int]]
particiones3 n = aux n n
  where
    aux 0 _ = [[]]
    aux _ 0 = []
    aux k m = do
      x <- [min k m, min k m - 1 .. 1]
      rest <- aux (k - x) x
      return (x:rest)

-- 4ª solución: Memoización con listas infinitas
-- =============================================

particiones4 :: Int -> [[Int]]
particiones4 n = aux !! n
  where
    aux = [] : map particiones [1..]
    particiones m = [m] : [x:p | x <- [m,m-1..1],
                                 p <- aux !! (m-x),
                                 x >= head p]

-- 5ª solución: Programación dinámica con listas
-- =============================================

particiones5 :: Int -> [[Int]]
particiones5 n = tabla !! n !! n
  where
    -- tabla !! m !! k es la lista de particiones de 'k'
    -- con sumandos no mayores a 'm'
    tabla = [[aux m k | k <- [0..n]] | m <- [0..n]]
    aux _ 0 = [[]]
    aux 0 _ = []
    aux m k
      | m > k     = tabla !! k !! k
      | otherwise = [m:p | p <- tabla !! m !! (k-m)] ++ tabla !! (m-1) !! k

-- 6ª solución: Programación dinámica con arrays
-- =============================================

particiones6 :: Int -> [[Int]]
particiones6 n = tabla ! (n, n)
  where
    tabla = listArray ((0,0), (n,n)) [aux m k | m <- [0..n], k <- [0..n]]
    aux _ 0 = [[]]
    aux 0 _ = []
    aux m k
      | m > k     = tabla ! (k, k)
      | otherwise = [m:p | p <- tabla ! (m, k-m)] ++ tabla ! (m-1, k)

-- 7ª solución: Uso de librería especializada (combinat)
-- =====================================================

particiones7 :: Int -> [[Int]]
particiones7 n = reverse (map fromPartition (partitions n))

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> [[Int]]) -> Spec
specG particiones = do
  it "e1" $
     particiones 4  `shouldBe`
     [[4],[3,1],[2,2],[2,1,1],[1,1,1,1]]
  it "e2" $
     particiones 5  `shouldBe`
     [[5],[4,1],[3,2],[3,1,1],[2,2,1],[2,1,1,1],[1,1,1,1,1]]

spec :: Spec
spec = do
  describe "def. 1" $ specG particiones1
  describe "def. 2" $ specG particiones2
  describe "def. 3" $ specG particiones3
  describe "def. 4" $ specG particiones4
  describe "def. 5" $ specG particiones5
  describe "def. 6" $ specG particiones6
  describe "def. 7" $ specG particiones7

-- La verificación es
--    λ> verifica
--    14 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_particiones :: Positive Int -> Bool
prop_particiones (Positive n) =
  all (== particiones1 n)
      [ particiones2 n
      , particiones3 n
      , particiones4 n
      , particiones5 n
      , particiones6 n
      , particiones7 n
      ]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=20}) prop_particiones
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia                                        --
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> length (particiones1 23)
--    1255
--    (11.38 secs, 7,316,678,920 bytes)
--    λ> length (particiones2 23)
--    1255
--    (0.04 secs, 7,380,536 bytes)
--    λ> length (particiones3 23)
--    1255
--    (0.04 secs, 7,502,064 bytes)
--    λ> length (particiones4 23)
--    1255
--    (0.01 secs, 3,453,280 bytes)
--    λ> length (particiones5 23)
--    1255
--    (0.01 secs, 1,345,992 bytes)
--    λ> length (particiones6 23)
--    1255
--    (0.01 secs, 1,350,008 bytes)
--    λ> length (particiones7 23)
--    1255
--    (0.01 secs, 2,124,360 bytes)
--
--    λ> length (particiones2 50)
--    204226
--    (2.54 secs, 1,633,031,680 bytes)
--    λ> length (particiones3 50)
--    204226
--    (2.56 secs, 1,700,791,432 bytes)
--    λ> length (particiones4 50)
--    204226
--    (2.43 secs, 863,579,104 bytes)
--    λ> length (particiones5 50)
--    204226
--    (0.16 secs, 76,997,688 bytes)
--    λ> length (particiones6 50)
--    204226
--    (0.19 secs, 77,020,880 bytes)
--    λ> length (particiones7 50)
--    204226
--    (0.36 secs, 377,769,904 bytes)
--
--    λ> length (particiones5 60)
--    966467
--    (0.84 secs, 358,027,072 bytes)
--    λ> length (particiones6 60)
--    966467
--    (0.87 secs, 358,061,664 bytes)
--    λ> length (particiones7 60)
--    966467
--    (2.00 secs, 1,975,593,104 bytes)
--
--    λ> length (particiones5 70)
--    4087968
--    (3.84 secs, 1,507,357,120 bytes)
--    λ> length (particiones6 70)
--    4087968
--    (3.64 secs, 1,507,405,400 bytes)
