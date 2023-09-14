-- Levenshtein.hs
-- La distancia Levenshtein (con programación dinámica)
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-octubre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La distancia de Levenshtein (o distancia de edición) es el número
-- mínimo de operaciones requeridas para transformar una cadena de
-- caracteres en otra. Las operaciones de edición que se pueden hacer
-- son:
-- + insertar un carácter (por ejemplo, de "abc" a "abca")
-- + eliminar un carácter (por ejemplo, de "abc" a "ac")
-- + sustituir un carácter (por ejemplo, de "abc" a "adc")
--
-- Por ejemplo, la distancia de Levenshtein entre "casa" y "calle" es de
-- 3 porque se necesitan al menos tres ediciones elementales para
-- cambiar uno en el otro:
--    "casa"  --> "cala"  (sustitución de 's' por 'l')
--    "cala"  --> "calla" (inserción de 'l' entre 'l' y 'a')
--    "calla" --> "calle" (sustitución de 'a' por 'e')
--
-- Definir la función
--    levenshtein :: String -> String -> Int
-- tal que (levenshtein xs ys) es la distancia de Levenshtein entre xs e
-- ys. Por ejemplo,
--    levenshtein "casa"  "calle"    ==  3
--    levenshtein "calle" "casa"     ==  3
--    levenshtein "casa"  "casa"     ==  0
--    levenshtein "ana" "maria"      ==  3
--    levenshtein "agua" "manantial" ==  7
-- ---------------------------------------------------------------------

module Levenshtein where

import Data.Array(Array, (!), array)
import Test.Hspec (Spec, hspec, it, shouldBe)

-- 1ª definición (por recursión)
-- =============================

levenshtein1 :: String -> String -> Int
levenshtein1 "" ys = length ys
levenshtein1 xs "" = length xs
levenshtein1 c1@(x:xs) c2@(y:ys)
  | x == y    = levenshtein1 xs ys
  | otherwise = 1 + minimum [ levenshtein1 xs c2
                            , levenshtein1 c1 ys
                            , levenshtein1 xs ys]

-- 2ª definición (con programación dinámica)
-- =========================================

levenshtein2 :: String -> String -> Int
levenshtein2 xs ys = matrizLevenshtein xs ys ! (m,n)
  where  m = length xs
         n = length ys

-- (matrizLevenshtein xs ys) es la matriz cuyo número de filas es la
-- longitud de xs, cuyo número de columnas es la longitud de ys y en
-- valor en la posición (i,j) es la distancia de Levenshtein entre los
-- primeros i caracteres de xs y los j primeros caracteres de ys. Por
-- ejemplo,
--    λ> elems (matrizLevenshtein "casa" "calle")
--    [0,1,2,3,4,5,1,0,1,2,3,4,2,1,0,1,2,3,3,2,1,1,2,3,4,3,2,2,2,3]
-- Gráficamente,
--       c a l l e
--     0,1,2,3,4,5,
--  c  1,0,1,2,3,4,
--  a  2,1,0,1,2,3,
--  s  3,2,1,1,2,3,
--  a  4,3,2,2,2,3
matrizLevenshtein :: String -> String -> Array (Int,Int) Int
matrizLevenshtein xs ys = q where
  q = array ((0,0),(m,n)) [((i,j), f i j) | i <- [0..m], j <- [0..n]]
  m = length xs
  n = length ys
  f 0 j = j
  f i 0 = i
  f i j | xs !! (i-1) == ys !! (j-1) = q ! (i-1,j-1)
        | otherwise                  = 1 + minimum [ q ! (i-1,j)
                                                   , q ! (i,j-1)
                                                   , q ! (i-1,j-1)]

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> levenshtein1 (show (2^33)) (show (3^33))
--    12
--    (16.19 secs, 11,766,254,536 bytes)
--    λ> levenshtein2 (show (2^33)) (show (3^33))
--    12
--    (0.02 secs, 0 bytes)

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "ej1" $
    levenshtein1 "casa"  "calle"    `shouldBe`  3
  it "ej2" $
    levenshtein1 "calle" "casa"     `shouldBe`  3
  it "ej3" $
    levenshtein1 "casa"  "casa"     `shouldBe`  0
  it "ej4" $
    levenshtein1 "ana" "maria"      `shouldBe`  3
  it "ej5" $
    levenshtein1 "agua" "manantial" `shouldBe`  7
  it "ej6" $
    levenshtein2 "casa"  "calle"    `shouldBe`  3
  it "ej7" $
    levenshtein2 "calle" "casa"     `shouldBe`  3
  it "ej8" $
    levenshtein2 "casa"  "casa"     `shouldBe`  0
  it "ej9" $
    levenshtein2 "ana" "maria"      `shouldBe`  3
  it "ej10" $
    levenshtein2 "agua" "manantial" `shouldBe`  7

-- La verificación es
--    λ> verifica
--
--    ej1
--    ej2
--    ej3
--    ej4
--    ej5
--    ej6
--    ej7
--    ej8
--    ej9
--    ej10
--
--    Finished in 0.0024 seconds
--    10 examples, 0 failures
