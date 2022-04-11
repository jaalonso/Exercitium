-- Alergias.hs
-- Código de las alergias.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 11-abril-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Para la determinación de las alergia se utiliza los siguientes
-- códigos para los alérgenos:
--    Huevos ........   1
--    Cacahuetes ....   2
--    Mariscos ......   4
--    Fresas ........   8
--    Tomates .......  16
--    Chocolate .....  32
--    Polen .........  64
--    Gatos ......... 128
-- Así, si Juan es alérgico a los cacahuetes y al chocolate, su
-- puntuación es 34 (es decir, 2+32).
--
-- Los alérgenos se representan mediante el siguiente tipo de dato
--   data Alergeno = Huevos
--                 | Cacahuetes
--                 | Mariscos
--                 | Fresas
--                 | Tomates
--                 | Chocolate
--                 | Polen
--                 | Gatos
--     deriving (Enum, Eq, Show, Bounded)
--
-- Definir la función
--    alergias :: Int -> [Alergeno]
-- tal que (alergias n) es la lista de alergias correspondiente a una
-- puntuación n. Por ejemplo,
--    λ> alergias 1
--    [Huevos]
--    λ> alergias 2
--    [Cacahuetes]
--    λ> alergias 3
--    [Huevos,Cacahuetes]
--    λ> alergias 5
--    [Huevos,Mariscos]
--    λ> alergias 255
--    [Huevos,Cacahuetes,Mariscos,Fresas,Tomates,Chocolate,Polen,Gatos]
-- ---------------------------------------------------------------------

module Alergias where

import Data.List (subsequences)
import Test.QuickCheck

data Alergeno =
    Huevos
  | Cacahuetes
  | Mariscos
  | Fresas
  | Tomates
  | Chocolate
  | Polen
  | Gatos
  deriving (Enum, Eq, Show, Bounded)

-- 1ª solución
-- ===========

alergias1 :: Int -> [Alergeno]
alergias1 n =
  [a | (a,c) <- zip alergenos codigos, c `elem` descomposicion n]

-- codigos es la lista de los códigos de los alergenos.
codigos :: [Int]
codigos = [2^x| x <- [0..7]]

-- (descomposicion n) es la descomposición de n como sumas de potencias
-- de 2. Por ejemplo,
--    descomposicion 3    ==  [1,2]
--    descomposicion 5    ==  [1,4]
--    descomposicion 248  ==  [8,16,32,64,128]
--    descomposicion 255  ==  [1,2,4,8,16,32,64,128]
descomposicion :: Int -> [Int]
descomposicion n =
  head [xs | xs <- subsequences codigos, sum xs == n]

-- 2ª solución
-- ===========

alergias2 :: Int -> [Alergeno]
alergias2 = map toEnum . codigosAlergias

-- (codigosAlergias n) es la lista de códigos de alergias
-- correspondiente a una puntuación n. Por ejemplo,
--    codigosAlergias 1  ==  [0]
--    codigosAlergias 2  ==  [1]
--    codigosAlergias 3  ==  [0,1]
--    codigosAlergias 4  ==  [2]
--    codigosAlergias 5  ==  [0,2]
--    codigosAlergias 6  ==  [1,2]
codigosAlergias :: Int -> [Int]
codigosAlergias = aux [0..7]
  where aux []     _             = []
        aux (x:xs) n | odd n     = x : aux xs (n `div` 2)
                     | otherwise = aux xs (n `div` 2)

-- 3ª solución
-- ===========

alergias3 :: Int -> [Alergeno]
alergias3 = map toEnum . codigosAlergias3

codigosAlergias3 :: Int -> [Int]
codigosAlergias3 n =
  [x | (x,y) <- zip [0..7] (int2bin n), y == 1]

-- (int2bin n) es la representación binaria del número n. Por ejemplo,
--    int2bin 10  ==  [0,1,0,1]
-- ya que 10 = 0*1 + 1*2 + 0*4 + 1*8
int2bin :: Int -> [Int]
int2bin n | n < 2     = [n]
          | otherwise = n `rem` 2 : int2bin (n `div` 2)

-- 4ª solución
-- ===========

alergias4 :: Int -> [Alergeno]
alergias4 = map toEnum . codigosAlergias4

codigosAlergias4 :: Int -> [Int]
codigosAlergias4 n =
  map fst (filter ((== 1) . snd) (zip  [0..7] (int2bin n)))

-- 5ª solución
-- ===========

alergias5 :: Int -> [Alergeno]
alergias5 = map (toEnum . fst)
          . filter ((1 ==) . snd)
          . zip [0..7]
          . int2bin

-- 6ª solución
-- ===========

alergias6 :: Int -> [Alergeno]
alergias6 = aux alergenos
  where aux []     _             = []
        aux (x:xs) n | odd n     = x : aux xs (n `div` 2)
                     | otherwise = aux xs (n `div` 2)

-- alergenos es la lista de los alergenos. Por ejemplo.
--    take 3 alergenos  ==  [Huevos,Cacahuetes,Mariscos]
alergenos :: [Alergeno]
alergenos = [minBound..maxBound]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_alergias :: Property
prop_alergias =
  forAll (arbitrary `suchThat` esValido) $ \n ->
  all (== alergias1 n)
      [alergias2 n,
       alergias3 n,
       alergias4 n,
       alergias5 n,
       alergias6 n]
  where esValido x = 1 <= x && x <= 255

-- La comprobación es
--    λ> quickCheck prop_alergias
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> last (map alergias1 [1..255])
--    [Huevos,Cacahuetes,Mariscos,Fresas,Tomates,Chocolate,Polen,Gatos]
--    (0.02 secs, 1,657,912 bytes)
--    λ> last (map alergias2 [1..255])
--    [Huevos,Cacahuetes,Mariscos,Fresas,Tomates,Chocolate,Polen,Gatos]
--    (0.01 secs, 597,080 bytes)
--    λ> last (map alergias3 [1..255])
--    [Huevos,Cacahuetes,Mariscos,Fresas,Tomates,Chocolate,Polen,Gatos]
--    (0.01 secs, 597,640 bytes)
--    λ> last (map alergias4 [1..255])
--    [Huevos,Cacahuetes,Mariscos,Fresas,Tomates,Chocolate,Polen,Gatos]
--    (0.01 secs, 598,152 bytes)
--    λ> last (map alergias5 [1..255])
--    [Huevos,Cacahuetes,Mariscos,Fresas,Tomates,Chocolate,Polen,Gatos]
--    (0.01 secs, 596,888 bytes)
