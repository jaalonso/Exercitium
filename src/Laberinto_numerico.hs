-- Laberinto_numerico.hs
-- Laberinto numérico.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 25-Julio-2014 (actualizado 11-Octubre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El problema del laberinto numérico consiste en, dados un par de
-- números enteros positivos, encontrar la longitud del camino más corto
-- entre ellos usando sólo las siguientes operaciones:
-- + multiplicar por 2,
-- + dividir por 2 (sólo para los pares) y
-- + sumar 2.
--
-- Por ejemplo, un camino mínimo
-- + de  3 a 12 es [3,6,12],
-- + de 12 a  3 es [12,6,3],
-- + de  9 a  2 es [9,18,20,10,12,6,8,4,2] y
-- + de  2 a  9 es [2,4,8,16,18,9].
--
-- Definir la función
--    longitudCaminoMinimo :: Int -> Int -> Int
-- tal que (longitudCaminoMinimo x y) es la longitud del camino mínimo
-- desde x hasta y en el laberinto numérico.
--    longitudCaminoMinimo 3 12  ==  2
--    longitudCaminoMinimo 12 3  ==  2
--    longitudCaminoMinimo 9 2   ==  8
--    longitudCaminoMinimo 2 9   ==  5
-- ---------------------------------------------------------------------

module Laberinto_numerico where

import Data.List (sort, nub)
import qualified Data.Set as Set
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========

longitudCaminoMinimo1 :: Int -> Int -> Int
longitudCaminoMinimo1 x y =
  head [n | n <- [0..], y `elem` orbita n [x]]

-- (orbita n xs) es el conjunto de números que se pueden obtener aplicando
-- como máximo n veces las operaciones a los elementos de xs. Por ejemplo,
--    orbita 0 [12]  ==  [12]
--    orbita 1 [12]  ==  [6,12,14,24]
--    orbita 2 [12]  ==  [3,6,7,8,12,14,16,24,26,28,48]
orbita :: Int -> [Int] -> [Int]
orbita 0 xs = sort xs
orbita n xs = sort (nub (ys ++ concat [sucesores x | x <- ys]))
  where ys = orbita (n-1) xs

-- (sucesores x) es la lista de los sucesores de x; es decir, los
-- números obtenidos aplicándole la operaciones a x. Por ejemplo,
--    sucesores 3 == [6,5]
--    sucesores 4 == [8,6,2]
sucesores :: Int -> [Int]
sucesores x = [2*x, x+2] ++ [x `div` 2 | even x]

-- 2ª solución
-- ===========

longitudCaminoMinimo2 :: Int -> Int -> Int
longitudCaminoMinimo2 x y
  | x == y    = 0
  | otherwise = anchura [(x, 0)] (Set.singleton x)
  where
    anchura [] _ = -1
    anchura ((nodo, dist):cola) visitados
      | nodo == y = dist
      | otherwise = anchura (cola ++ [(n, dist + 1) | n <- nuevos])
                            (foldr Set.insert visitados nuevos)
      where
        nuevos = filter (`Set.notMember` visitados) (sucesores nodo)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: (Int -> Int -> Int) -> Spec
specG longitudCaminoMinimo = do
  it "e1" $
    longitudCaminoMinimo 3 12 `shouldBe` 2
  it "e2" $
    longitudCaminoMinimo 12 3 `shouldBe` 2
  it "e3" $
    longitudCaminoMinimo 9 2  `shouldBe` 8
  it "e4" $
    longitudCaminoMinimo 2 9  `shouldBe` 5

spec :: Spec
spec = do
  describe "def. 1" $ specG longitudCaminoMinimo1
  describe "def. 2" $ specG longitudCaminoMinimo2

-- La verificación es
--    λ> verifica
--    8 examples, 0 failures

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_longitudCaminoMinimo :: Positive Int -> Positive Int -> Bool
prop_longitudCaminoMinimo (Positive x) (Positive y) =
  longitudCaminoMinimo1 x y == longitudCaminoMinimo2 x y

-- La comprobación es
--    λ> quickCheck prop_longitudCaminoMinimo
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> longitudCaminoMinimo1 1 511
--    17
--    (2.29 secs, 58,152,384 bytes)
--    λ> longitudCaminoMinimo2 1 511
--    17
--    (0.20 secs, 683,548,744 bytes)
