-- Mastermind.hs
-- Mastermind.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 25-Abril-2014
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El Mastermind es un juego que consiste en deducir un código
-- numérico formado por una lista de números distintos. Cada vez que se
-- empieza una partida, el programa debe elegir un código, que será lo
-- que el jugador debe adivinar en la menor cantidad de intentos
-- posibles. Cada intento consiste en una propuesta de un código posible
-- que propone el jugador, y una respuesta del programa. Las respuestas
-- le darán pistas al jugador para que pueda deducir el código.
--
-- Estas pistas indican cuán cerca estuvo el número propuesto de la
-- solución a través de dos valores: la cantidad de aciertos es la
-- cantidad de dígitos que propuso el jugador que también están en el
-- código en la misma posición. La cantidad de coincidencias es la
-- cantidad de digitos que propuso el jugador que también están en el
-- código pero en una posición distinta.
--
-- Por ejemplo, si el código que eligió el programa es el [2,6,0,7], y
-- el jugador propone el [1,4,0,6], el programa le debe responder un
-- acierto (el 0, que está en el código original en el mismo lugar, el
-- tercero), y una coincidencia (el 6, que también está en el código
-- original, pero en la segunda posición, no en el cuarto como fue
-- propuesto). Si el jugador hubiera propuesto el [3,5,9,1], habría
-- obtenido como respuesta ningún acierto y ninguna coincidencia, ya que
-- no hay números en común con el código original, y si se obtienen
-- cuatro aciertos es porque el jugador adivinó el código y ganó el
-- juego.
--
-- Definir la función
--    mastermind :: [Int] -> [Int] -> (Int,Int)
-- tal que (mastermind xs ys) es el par formado por los números de
-- aciertos y de coincidencias entre xs e ys. Por ejemplo,
--    mastermind [2,6,0,7] [1,4,0,6]  ==  (1,1)
--    mastermind [2,6,0,7] [3,5,9,1]  ==  (0,0)
--    mastermind [2,6,0,7] [1,6,0,4]  ==  (2,0)
--    mastermind [2,6,0,7] [2,6,0,7]  ==  (4,0)
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Mastermind where

import Data.List (nub)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

-- 1ª solución
-- ===========
mastermind1 :: [Int] -> [Int] -> (Int,Int)
mastermind1 xs ys =
  (length (aciertos xs ys),length (coincidencias xs ys))

-- (aciertos xs ys) es la lista de aciertos entre xs e ys. Por ejemplo,
--    aciertos [2,6,0,7] [1,4,0,6]  ==  [0]
aciertos :: Eq a => [a] -> [a] -> [a]
aciertos xs ys =
  [x | (x,y) <- zip xs ys, x == y]

-- (coincidencia xs ys) es la lista de coincidencias entre xs e ys. Por
-- ejemplo,
--    coincidencias [2,6,0,7] [1,4,0,6]  ==  [6]
coincidencias :: Eq a => [a] -> [a] -> [a]
coincidencias xs ys =
  [x | x <- xs, x `elem` ys, x `notElem` zs]
  where zs = aciertos xs ys

-- 2ª solución
-- ===========
mastermind2 :: [Int] -> [Int] -> (Int,Int)
mastermind2 xs ys = aux xs ys
  where aux [] [] = (0,0)
        aux (x:xs') (z:zs)
          | x == z      = (a+1,b)
          | x `elem` ys = (a,b+1)
          | otherwise   = (a,b)
          where (a,b) = aux xs' zs

-- 3ª solución
-- ===========
mastermind3 :: [Int] -> [Int] -> (Int,Int)
mastermind3 xs ys = (nAciertos,nCoincidencias)
  where nAciertos = length [(x,y) | (x,y) <- zip xs ys, x == y]
        nCoincidencias = length (xs++ys) - length (nub (xs++ys)) - nAciertos

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Int] -> [Int] -> (Int,Int))  -> Spec
specG mastermind = do
  it "e1" $
    mastermind [2,6,0,7] [1,4,0,6] `shouldBe` (1,1)
  it "e2" $
    mastermind [2,6,0,7] [3,5,9,1] `shouldBe` (0,0)
  it "e3" $
    mastermind [2,6,0,7] [1,6,0,4] `shouldBe` (2,0)
  it "e4" $
    mastermind [2,6,0,7] [2,6,0,7] `shouldBe` (4,0)

spec :: Spec
spec = do
  describe "def. 1" $ specG mastermind1
  describe "def. 2" $ specG mastermind2
  describe "def. 3" $ specG mastermind3

-- La verificación es
--    λ> verifica
--    12 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

-- Generador para listas de 4 elementos elgidos del 1 al 6. Por ejemplo,
--    λ> sample genMastermind
--    [2,4,6,1]
--    [6,5,1,2]
--    [3,2,6,4]
--    [1,3,2,5]
--    [5,2,6,4]
--    [3,2,4,1]
--    [2,6,5,4]
--    [4,1,2,3]
--    [6,4,2,5]
--    [4,5,3,2]
--    [6,3,4,2]
genMastermind :: Gen [Int]
genMastermind = do
  elementos <- shuffle [1..6]
  return (take 4 elementos)

-- Generador para el juego clásico de Mastermind (4 posiciones, 6
-- colores). Por ejemplo,
--    λ> sample genParesMastermind
--    ([3,5,6,2],[6,3,4,2])
--    ([1,4,3,2],[4,2,1,3])
--    ([1,5,3,6],[2,6,4,1])
--    ([5,6,1,4],[1,6,4,2])
--    ([2,6,4,3],[3,6,4,1])
--    ([6,1,3,5],[6,1,3,5])
--    ([6,4,5,1],[3,2,4,5])
--    ([5,6,4,2],[1,4,6,2])
--    ([1,5,3,6],[3,4,2,6])
--    ([6,1,5,3],[1,4,5,6])
--    ([1,2,4,5],[4,1,2,5])
genParesMastermind :: Gen ([Int], [Int])
genParesMastermind = do
  xs <- genMastermind
  ys <- genMastermind
  return (xs, ys)

prop_Mastermind :: Property
prop_Mastermind = forAll genParesMastermind $ \(xs, ys) ->
  all (== mastermind1 xs ys)
      [mastermind2 xs ys,
       mastermind3 xs ys]

-- La comprobación es
--    λ> quickCheck prop_Mastermind
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> mastermind1 [1..20000] [1,3..40000]
--    (1,9999)
--    (2.14 secs, 12,442,128 bytes)
--    λ> mastermind2 [1..20000] [1,3..40000]
--    (1,9999)
--    (2.01 secs, 11,969,360 bytes)
--    λ> mastermind3 [1..20000] [1,3..40000]
--    (1,9999)
--    (5.60 secs, 11,322,176 bytes)
--
--    λ> mastermind1 [1..20000] [1..20000]
--    (20000,0)
--    (5.02 secs, 15,962,800 bytes)
--    λ> mastermind2 [1..20000] [1..20000]
--    (20000,0)
--    (0.02 secs, 12,978,344 bytes)
--    λ> mastermind3 [1..20000] [1..20000]
--    (20000,0)
--    (3.89 secs, 12,122,840 bytes)
