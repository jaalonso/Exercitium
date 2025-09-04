-- Sopa_de_letras.hs
-- Sopa de letras.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 26-Junio-2014 (actualizado 3-Septiembre-2025)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las matrices se puede representar mediante tablas cuyos índices son
-- pares de números naturales:
--    type Matriz a = Array (Int,Int) a
--
-- Definir la función
--    enLaSopa :: Eq a => [a] -> Matriz a -> Bool
-- tal que (enLaSopa c p) se verifica si c está en la matriz p en
-- horizontal o en vertical. Por ejemplo, si ej1 es la matriz siguiente:
--    ej1 :: Matriz Char
--    ej1 = listaMatriz ["mjtholueq",
--                       "juhoolauh",
--                       "dariouhyj",
--                       "rngkploaa"]
-- donde la función listaMatriz está definida por
--    listaMatriz :: [[a]] -> Matriz a
--    listaMatriz xss = listArray ((1,1),(m,n)) (concat xss)
--      where m = length xss
--            n = length (head xss)
-- entonces,
--    enLaSopa "dar"  ej1  ==  True   -- En horizontal a la derecha en la 3ª fila
--    enLaSopa "oir"  ej1  ==  True   -- En horizontal a la izquierda en la 3ª fila
--    enLaSopa "juan" ej1  ==  True   -- En vertical descendente en la 2ª columna
--    enLaSopa "kio"  ej1  ==  True   -- En vertical ascendente en la 3ª columna
--    enLaSopa "Juan" ej1  ==  False
--    enLaSopa "hola" ej1  ==  False
-- ---------------------------------------------------------------------

module Sopa_de_letras where

import Data.Array (Array, (!), bounds, listArray)
import Data.List (isInfixOf, transpose)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck

type Matriz a = Array (Int,Int) a

listaMatriz :: [[a]] -> Matriz a
listaMatriz xss = listArray ((1,1),(m,n)) (concat xss)
  where m = length xss
        n = length (head xss)

ej1 :: Matriz Char
ej1 = listaMatriz ["mjtholueq",
                   "juhoolauh",
                   "dariouhyj",
                   "rngkploaa"]

-- 1ª solución
-- ===========

enLaSopa1 :: Eq a => [a] -> Matriz a -> Bool
enLaSopa1 c p =
    or [c `isInfixOf` xs |
        xs <- [[p!(i,j) | j <- [1..n]]     | i <- [1..m]] ++
              [[p!(i,j) | j <- [n,n-1..1]] | i <- [1..m]] ++
              [[p!(i,j) | i <- [1..m]]     | j <- [1..n]] ++
              [[p!(i,j) | i <- [m,m-1..1]] | j <- [1..n]]]
    where (_,(m,n)) = bounds p

-- 2ª solución
-- ===========

enLaSopa2 :: Eq a => [a] -> Matriz a -> Bool
enLaSopa2 c p = estaEnHorizontal c p || estaEnVertical c p

-- (numFilas p) es el número de filas de la matriz p. Por ejemplo,
--    numFilas ej1  ==  4
numFilas :: Matriz a -> Int
numFilas = fst . snd . bounds

-- (numColumnas p) es el número de columnas de la matriz p. Por ejemplo,
--    numColumnas ej1  ==  9
numColumnas :: Matriz a -> Int
numColumnas = snd . snd . bounds

-- (fila i p) es la fila i-ésima de la matriz p. Por ejemplo,
--    fila 2 ej1  ==  "juhoolauh"
fila :: Int -> Matriz a -> [a]
fila i p =
  [p!(i,j) | j <- [1..n]]
  where n = numColumnas p

-- (columna j p) es la columna j-ésima de la matriz p. Por ejemplo,
--    columna 2 ej1  ==  "juan"
columna :: Int -> Matriz a -> [a]
columna j p =
  [p!(i,j) | i <- [1..m]]
  where m = numFilas p

-- (filas p) es la lista de las filas de la matriz p. Por ejemplo,
--    λ> filas ej1
--    ["mjtholueq","juhoolauh","dariouhyj","rngkploaa"]
filas :: Matriz a -> [[a]]
filas p =
  [fila i p | i <- [1..numFilas p]]

-- (columnas p) es la lista de las columnas de la matriz p. Por ejemplo,
--    λ> columnas ej1
--    ["mjdr","juan","thrg","hoik","ooop","llul","uaho","euya","qhja"]
columnas :: Matriz a -> [[a]]
columnas p =
  [columna j p | j <- [1..numColumnas p]]

-- (estaEnHorizontal c p) se verifica si c está en la matriz p en
-- horizontal. Por ejemplo,
--    estaEnHorizontal "dar" ej1  ==  True
--    estaEnHorizontal "oir" ej1  ==  True
--    estaEnHorizontal "juan" ej1 ==  False
estaEnHorizontal :: Eq a => [a] -> Matriz a -> Bool
estaEnHorizontal c p =
  or [c `isInfixOf` xs | xs <- filas p ++ map reverse (filas p)]

-- (estaEnVertical c p) se verifica si c está en la matriz p en
-- vertical. Por ejemplo,
--    estaEnVertical "juan" ej1  ==  True
--    estaEnVertical "kio"  ej1  ==  True
--    estaEnVertical "dar"  ej1  ==  False
estaEnVertical :: Eq a => [a] -> Matriz a -> Bool
estaEnVertical c p =
  or [c `isInfixOf ` xs | xs <- columnas p ++ map reverse (columnas p)]

-- 3ª solución
-- ===========

enLaSopa3 :: Eq a => [a] -> Matriz a -> Bool
enLaSopa3 c p = any (c `isInfixOf`) lineas
  where
    fs = filas p
    cs = transpose fs
    lineas = fs ++ map reverse fs ++ cs ++ map reverse cs

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

specG :: ([Char] -> Matriz Char -> Bool) -> Spec
specG enLaSopa = do
  it "e1" $
    enLaSopa "dar"  ej1  `shouldBe`  True
  it "e2" $
    enLaSopa "oir"  ej1  `shouldBe`  True
  it "e3" $
    enLaSopa "juan" ej1  `shouldBe`  True
  it "e4" $
    enLaSopa "kio"  ej1  `shouldBe`  True
  it "e5" $
    enLaSopa "Juan" ej1  `shouldBe`  False
  it "e6" $
    enLaSopa "hola" ej1  `shouldBe`  False

spec :: Spec
spec = do
  describe "def. 1"  $ specG enLaSopa1
  describe "def. 2"  $ specG enLaSopa2
  describe "def. 3"  $ specG enLaSopa3

-- La verificación es
--    λ> verifica
--    18 examples, 0 failures

-- Equivalencia de las definiciones
-- ================================

newtype Matriz2 = M (Array (Int,Int) Int)
  deriving Show

-- Generador de matrices arbitrarias. Por ejemplo,
--    λ> generate matrizArbitraria
--    M (array ((1,1),(3,4))
--             [((1,1),18),((1,2),6), ((1,3),-23),((1,4),-13),
--              ((2,1),-2),((2,2),22),((2,3),-25),((2,4),-5),
--              ((3,1),2), ((3,2),16),((3,3),-15),((3,4),7)])
matrizArbitraria :: Gen Matriz2
matrizArbitraria = do
  m  <- chooseInt (1,10)
  n  <- chooseInt (1,10)
  xs <- vectorOf (m*n) arbitrary
  return (M (listArray ((1,1),(m,n)) xs))

-- Matriz es una subclase de Arbitrary.
instance Arbitrary Matriz2 where
  arbitrary = matrizArbitraria

-- La propiedad es
prop_enLaSopa :: [Int] -> Matriz2 -> Bool
prop_enLaSopa xs (M p) =
  all (== enLaSopa1 xs p)
      [enLaSopa2 xs p,
       enLaSopa3 xs p]

-- La comprobación es
--    λ> quickCheck prop_enLaSopa
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> ejemplo = listaMatriz (replicate 1000 (replicate 1000 'a'))
--    λ> enLaSopa1 "b" ejemplo
--    False
--    (1.88 secs, 1,458,976,016 bytes)
--    λ> enLaSopa2 "b" ejemplo
--    False
--    (1.72 secs, 1,491,736,440 bytes)
--    λ> enLaSopa3 "b" ejemplo
--    False
--    (0.70 secs, 553,799,536 bytes)
