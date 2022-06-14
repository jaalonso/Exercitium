-- La_sucesion_del_reloj_astronomico_de_Praga.hs
-- La sucesión del reloj astronómico de Praga.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 20-junio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La cadena infinita "1234321234321234321...", formada por la
-- repetición de los dígitos 123432, tiene una propiedad (en la que se
-- basa el funcionamiento del reloj astronómico de Praga
-- http://bit.ly/1FnWCQs ): la cadena se puede partir en una sucesión
-- de números, de forma que la suma de los dígitos de dichos números
-- es la sucesión de los números naturales, como se observa a
-- continuación: 
--     1, 2, 3, 4, 32, 123, 43, 2123, 432, 1234, 32123, ...
--     1, 2, 3, 4,  5,   6,  7,    8,   9,   10,    11, ...
--
-- Definir la lista
--    reloj :: [Integer]
-- cuyos elementos son los términos de la sucesión anterior. Por
-- ejemplo, 
--    λ> take 11 reloj
--    [1,2,3,4,32,123,43,2123,432,1234,32123]
--    λ> (reloj !! 1000) `mod` (10^50)
--    23432123432123432123432123432123432123432123432123
--
-- Nota: La relación entre la sucesión y el funcionamiento del reloj se
-- puede ver en The Mathematics Behind Prague's Horloge Introduction 
-- http://www.global-sci.org/mc/galley/prague_sc_pic/prague_eng.pdf
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module La_sucesion_del_reloj_astronomico_de_Praga where

import Data.List (inits, tails)
import Data.Char (digitToInt)
import Test.QuickCheck (NonNegative (NonNegative), quickCheck)

-- 1ª solución
-- ===========

reloj1 :: [Integer]
reloj1 = aux [1..] (cycle "123432")
  where aux (n:ns) xs = read ys : aux ns zs
          where (ys,zs) = prefijoSuma n xs

-- (prefijoSuma n xs) es el par formado por el primer prefijo de xs cuyo
-- suma es n y el resto de xs. Por ejemplo,
--    prefijoSuma 6 "12343"  ==  ("123","43")
prefijoSuma :: Int -> String -> (String,String)
prefijoSuma n xs = 
  head [(us,vs) | (us,vs) <- zip (inits xs) (tails xs)
                , sumaD us == n]

-- (sumaD xs) es la suma de los dígitos de xs. Por ejemplo,
--    sumaD "123"  ==  6
sumaD :: String -> Int
sumaD = sum . map digitToInt

-- 2ª solución
-- ===========

reloj2 :: [Integer]
reloj2 = aux [1..] (cycle "123432")
  where aux (n:ns) xs = read ys : aux ns zs
          where (ys,zs) = prefijoSuma2 n xs

-- (prefijoSuma n xs) es el par formado por el primer prefijo de xs cuyo
-- suma es n y el resto de xs. Por ejemplo,
--    prefijoSuma2 6 "12343"  ==  ("123","43")
prefijoSuma2 :: Int -> String -> (String,String)
prefijoSuma2 n (x:xs) 
  | y == n = ([x],xs)
  | otherwise = (x:ys,zs) 
  where y       = read [x]
        (ys,zs) = prefijoSuma2 (n-y) xs

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_reloj :: NonNegative Int -> Bool
prop_reloj (NonNegative n) =
  reloj1 !! n == reloj2 !! n
  
-- La comprobación es
--    λ> quickCheck prop_reloj
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> (reloj1 !! 1000) `mod` (10^9)
--    123432123
--    (2.47 secs, 5,797,620,784 bytes)
--    λ> (reloj2 !! 1000) `mod` (10^9)
--    123432123
--    (0.44 secs, 798,841,528 bytes)

