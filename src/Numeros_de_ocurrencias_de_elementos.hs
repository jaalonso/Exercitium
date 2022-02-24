-- Numeros_de_ocurrencias_de_elementos.hs
-- Número de ocurrencias de elementos
-- José A. Alonso Jiménez
-- Sevilla, 7-febrero-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    ocurrenciasElementos :: Ord a => [a] -> [(a,Int)]
-- tal que (ocurrencias xs) es el conjunto de los elementos de xs junto
-- con sus números de ocurrencias. Por ejemplo,
--    ocurrenciasElementos1 [3,2,3,1,2,3,5,3] == [(3,4),(2,2),(1,1),(5,1)]
--    ocurrenciasElementos1 "tictac"          == [('t',2),('i',1),('c',2),('a',1)]
-- ---------------------------------------------------------------------

module Numeros_de_ocurrencias_de_elementos where

import Data.List (group, nub, sort)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Test.QuickCheck

-- 1ª solución
-- ===========

ocurrenciasElementos1 :: Ord a => [a] -> [(a,Int)]
ocurrenciasElementos1 xs =
  [(x,ocurrencias x xs) | x <- nub xs]

-- (ocurrencias x xs) es el número de ocurrencias de x en xs. Por
-- ejemplo,
--    ocurrencias 'a' "Salamanca"  ==  4
ocurrencias :: Ord a => a -> [a] -> Int
ocurrencias x xs = length (filter (==x) xs)

-- 2ª solución
-- ===========

ocurrenciasElementos2 :: Ord a => [a] -> [(a,Int)]
ocurrenciasElementos2 xs = map ocurrencias2 (nub xs)
  where ocurrencias2 x = (x,fromJust (lookup x (frecuencias xs)))

-- (frecuencias xs) es la lista ordenada de los elementos de xs juntos
-- con sus números de ocurrencias. Por ejemplo,
--    frecuencias [3,2,3,1,2,3,5,3]  ==  [(1,1),(2,2),(3,4),(5,1)]
frecuencias :: Ord a => [a] -> [(a, Int)]
frecuencias xs = [(head ys, length ys) | ys <- group (sort xs)]

-- 3ª solución
-- ===========

ocurrenciasElementos3 :: Ord a => [a] -> [(a,Int)]
ocurrenciasElementos3 xs = map ocurrencias3 (nub xs)
  where diccionario    = dicFrecuencias xs
        ocurrencias3 x = (x, diccionario M.! x)

-- (dicFrecuencias xs) es el diccionario de los elementos de xs juntos
-- con sus números de ocurrencias. Por ejemplo,
--    λ> dicFrecuencias [3,2,3,1,2,3,5,3]
--    fromList [(1,1),(2,2),(3,4),(5,1)]
dicFrecuencias :: Ord a => [a] -> M.Map a Int
dicFrecuencias xs = M.fromListWith (+) (zip xs (repeat 1))

-- 4ª solución
-- ===========

ocurrenciasElementos4 :: Ord a => [a] -> [(a,Int)]
ocurrenciasElementos4 = foldl aux []
  where
    aux [] y                     = [(y,1)]
    aux ((x,k):xs) y | x == y    = (x, k + 1) : xs
                     | otherwise = (x, k) : aux xs y

-- Equivalencia de las definiciones
-- ================================

-- La propiedad es
prop_ocurrenciasElementos :: [Integer] -> Bool
prop_ocurrenciasElementos xs =
  all (== (ocurrenciasElementos1 xs))
      [f xs | f <- [ocurrenciasElementos2,
                    ocurrenciasElementos3,
                    ocurrenciasElementos4]]

verifica_ocurrenciasElementos :: IO ()
verifica_ocurrenciasElementos = quickCheck prop_ocurrenciasElementos

-- La comprobación es
--    λ> quickCheck prop_ocurrenciasElementos
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last (ocurrenciasElementos1 (show (product [1..10^5])))
--    ('5',42935)
--    (7.93 secs, 11,325,169,512 bytes)
--    λ> last (ocurrenciasElementos2 (show (product [1..10^5])))
--    ('5',42935)
--    (8.46 secs, 11,750,911,592 bytes)
--    λ> last (ocurrenciasElementos3 (show (product [1..10^5])))
--    ('5',42935)
--    (8.29 secs, 11,447,015,896 bytes)
--    λ> last (ocurrenciasElementos4 (show (product [1..10^5])))
--    ('5',42935)
--    (9.97 secs, 12,129,527,912 bytes)

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

verifica :: (String -> [(Char,Int)]) -> IO ()
verifica ocurrenciasElementos = hspec $ do
  it "e1" $
    ocurrenciasElementos "abracadabra" `shouldBe`
    [('a',5),('b',2),('r',2),('c',1),('d',1)]
  it "e2" $
    ocurrenciasElementos "Code Wars"   `shouldBe`
    [('C',1),('o',1),('d',1),('e',1),(' ',1),('W',1),('a',1),('r',1),('s',1)]
