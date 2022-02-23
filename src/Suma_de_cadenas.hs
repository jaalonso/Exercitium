-- Suma_de_cadenas.hs
-- Suma de cadenas.
-- José A. Alonso Jiménez
-- Sevilla, 27-enero-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Definir la función
--    sumaCadenas :: String -> String -> String
-- tal que (sumaCadenas xs ys) es la cadena formada por el número entero
-- que es la suma de los números enteros cuyas cadenas que lo
-- representan son xs e ys; además, se supone que la cadena vacía
-- representa al cero. Por ejemplo,
--    sumaCadenas "2"   "6"  == "8"
--    sumaCadenas "14"  "2"  == "16"
--    sumaCadenas "14"  "-5" == "9"
--    sumaCadenas "-14" "-5" == "-19"
--    sumaCadenas "5"   "-5" == "0"
--    sumaCadenas ""    "5"  == "5"
--    sumaCadenas "6"   ""   == "6"
--    sumaCadenas ""    ""   == "0"
-- ---------------------------------------------------------------------

module Suma_de_cadenas where

-- 1ª solución
-- ===========

sumaCadenas1 :: String -> String -> String
sumaCadenas1 xs ys =
  show (sum (map read (filter (not . null) [xs, ys])))

-- 2ª solución
-- ===========

sumaCadenas2 :: String -> String -> String
sumaCadenas2 =
  ((show . sum . map read . filter (not . null)) .) . (. return) . (:)

-- 3ª solución
-- ===========

sumaCadenas3 :: String -> String -> String
sumaCadenas3 "" "" = "0"
sumaCadenas3 "" ys = ys
sumaCadenas3 xs "" = xs
sumaCadenas3 xs ys = show (read xs + read ys)

-- 4ª solución
-- ===========

sumaCadenas4 :: String -> String -> String
sumaCadenas4 xs ys = show (numero xs + numero ys)

-- (numero xs) es el número entero representado por la cadena xs
-- suponiendo que la cadena vacía representa al cero.. Por ejemplo,
--    numero "12"   ==  12
--    numero "-12"  ==  -12
--    numero "0"    ==  0
--    numero ""     ==  0
numero :: String -> Int
numero "" = 0
numero xs = read xs
