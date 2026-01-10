-- Listas_con_suma_dada.hs
-- Listas con suma dada.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-Enero-2015 (actualizado 5-Enero-2026)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    conSuma :: (Eq a, Num a) => [a] -> [[a]] -> [[[a]]]
-- tal que (conSuma xs yss) es la lista de las sublistas de yss cuya
-- suma, elemento a elemento, es xs. Por ejemplo,
--    λ> conSuma [9,10,12] [[4,7,3],[3,1,4],[5,3,9],[2,2,5]]
--    [[[4,7,3],[5,3,9]],[[4,7,3],[3,1,4],[2,2,5]]]
--    λ> conSuma [9,11,12] [[4,7,3],[3,1,4],[5,3,9],[2,2,5]]
--    []
--    λ> length (conSuma [5,5,5] (replicate 70 [1,1,1]))
--    12103014
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Listas_con_suma_dada where

import Data.List (sort, subsequences, transpose)
import qualified Data.Map.Strict as M
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

conSuma1 :: (Eq a, Num a) => [a] -> [[a]] -> [[[a]]]
conSuma1 xs yss = [zss | zss <- subsequences yss, suma1 zss == xs]

-- (suma xss) es la suma de las listas xs. Por ejemplo,
--    suma [[4,7,3],[3,1,4],[2,2,5]]  ==  [9,10,12
suma1 :: Num a => [[a]] -> [a]
suma1 []       = []
suma1 [xs]     = xs
suma1 (xs:xss) = zipWith (+) xs (suma1 xss)

-- 2ª solución
-- ===========

conSuma2 :: (Eq a, Num a) => [a] -> [[a]] -> [[[a]]]
conSuma2 xs yss = [zss | zss <- subsequences yss, suma2 zss == xs]

suma2 :: Num a => [[a]] -> [a]
suma2 = map sum . transpose

-- 3ª solución
-- ===========

conSuma3 :: (Eq a, Num a) => [a] -> [[a]] -> [[[a]]]
conSuma3 xs =
  filter ((xs==) . foldr1 (zipWith (+))) . tail . subsequences

-- 4ª solución
-- ===========

conSuma4 :: (Ord a, Num a) => [a] -> [[a]] -> [[[a]]]
conSuma4 _  [] = []
conSuma4 xs [ys] | xs == ys  = [[ys]]
                 | otherwise = []
conSuma4 xs (ys:yss)
    | xs == ys    = [ys] : conSuma4 xs yss
    | menor ys xs = [ys:zs | zs <- conSuma4 (menos xs ys) yss] ++
                    conSuma4 xs yss
    | otherwise   = conSuma4 xs yss
    where menor as bs = and (zipWith (<=) as bs)
          menos       = zipWith (-)

-- 5ª solución
-- ===========

conSuma5 :: (Eq a, Num a) => [a] -> [[a]] -> [[[a]]]
conSuma5 xs []
  | all (==0) xs = [[]]
  | otherwise    = []
conSuma5 xs (y:ys) =
  map (y:) (conSuma5 (zipWith (-) xs y) ys) ++ conSuma5 xs ys

-- 6ª solución
-- ===========

conSuma6 :: (Ord a, Num a) => [a] -> [[a]] -> [[[a]]]
conSuma6 objetivo yss = M.findWithDefault [] objetivo tablaFinal
  where
    -- El vector cero inicial (ej: [0,0,0])
    cero = replicate (length objetivo) 0

    -- Estado inicial: La suma cero se consigue con una lista vacía [[]]
    tablaInicial = M.singleton cero [[]]

    -- Procesamos cada vector uno a uno actualizando el mapa de sumas logradas
    tablaFinal = foldl actualizarTabla tablaInicial yss

    actualizarTabla tablaActual v =
      -- Combinamos la tabla actual con las nuevas sumas generadas al añadir 'v'
      M.unionWith (++) tablaActual nuevasSumas
      where
        nuevasSumas = M.fromListWith (++)
          [ (zipWith (+) s v, map (v:) caminos)
          | (s, caminos) <- M.toList tablaActual ]

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> [[Int]] -> [[[Int]]]) -> Spec
specG conSuma = do
  it "e1" $
    sort (map sort (conSuma [9,10,12] [[4,7,3],[3,1,4],[5,3,9],[2,2,5]]))
    `shouldBe` [[[2,2,5],[3,1,4],[4,7,3]],[[4,7,3],[5,3,9]]]
  it "e2" $
    conSuma [9,11,12] [[4,7,3],[3,1,4],[5,3,9],[2,2,5]]
    `shouldBe` []

spec :: Spec
spec = do
  describe "def. 1" $ specG conSuma1
  describe "def. 2" $ specG conSuma2
  describe "def. 3" $ specG conSuma3
  describe "def. 4" $ specG conSuma4
  describe "def. 5" $ specG conSuma5
  describe "def. 6" $ specG conSuma6

-- La verificación es
--    λ> verifica
--    12 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- -- Genera una matriz. Por ejemplo,
-- --    λ> generate genMatriz
-- --    [[15,22],[29,12],[-28,-1]]
-- genMatriz :: Gen [[Int]]
-- genMatriz = do
--   m <- choose (1, 20)
--   n <- choose (1, 20)
--   vectorOf m (vectorOf n (arbitrary :: Gen Int))
--
-- -- La propiedad es
-- prop_equivalencia :: Property
-- prop_equivalencia = forAll genMatriz $ \xss ->
--   all (== conSuma1 xss)
--       [conSuma2 xss,
--        conSuma4 xss,
--        conSuma5 xss,
--        conSuma6 xss,
--        conSuma7 xss]

-- La comprobación es
--    λ> quickCheck prop_equivalencia
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> :set +s
--    λ> length (conSuma1 [5,5,5] (replicate 20 [1,1,1]))
--    15504
--    (3.00 secs, 2,927,447,376 bytes)
--    λ> length (conSuma2 [5,5,5] (replicate 20 [1,1,1]))
--    15504
--    (1.02 secs, 2,327,481,752 bytes)
--    λ> length (conSuma3 [5,5,5] (replicate 20 [1,1,1]))
--    15504
--    (0.47 secs, 1,668,908,664 bytes)
--    λ> length (conSuma4 [5,5,5] (replicate 20 [1,1,1]))
--    15504
--    (0.04 secs, 28,411,608 bytes)
--    λ> length (conSuma5 [5,5,5] (replicate 20 [1,1,1]))
--    15504
--    (1.32 secs, 1,051,793,824 bytes)
--    λ> length (conSuma6 [5,5,5] (replicate 20 [1,1,1]))
--    15504
--    (0.02 secs, 5,678,848 bytes)
--
--    λ> length (conSuma3 [5,5,5] (replicate 22 [1,1,1]))
--    26334
--    (1.92 secs, 7,308,667,976 bytes)
--    λ> length (conSuma4 [5,5,5] (replicate 22 [1,1,1]))
--    26334
--    (0.06 secs, 45,888,808 bytes)
--    λ> length (conSuma6 [5,5,5] (replicate 22 [1,1,1]))
--    26334
--    (0.02 secs, 9,516,680 bytes)
--
--    λ> length (conSuma4 [5,5,5] (replicate 45 [1,1,1]))
--    1221759
--    (2.29 secs, 1,721,519,448 bytes)
--    λ> length (conSuma6 [5,5,5] (replicate 45 [1,1,1]))
--    1221759
--    (0.35 secs, 647,077,000 bytes)
