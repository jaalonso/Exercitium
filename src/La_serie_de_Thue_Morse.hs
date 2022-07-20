-- La_serie_de_Thue_Morse.hs
-- La serie de Thue-Morse.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 27-julio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- La [serie de Thue-Morse](http://bit.ly/1KvZONW) comienza con el
-- término [0] y sus siguientes términos se construyen añadiéndole al
-- anterior su complementario (es decir, la lista obtenida cambiando el
-- 0 por 1 y el 1 por 0). Los primeros términos de la serie son
--    [0]
--    [0,1]
--    [0,1,1,0]
--    [0,1,1,0,1,0,0,1]
--    [0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0]
-- 
-- Definir la lista
--    serieThueMorse :: [[Int]]
-- tal que sus elementos son los términos de la serie de Thue-Morse. Por
-- ejemplo, 
--    λ> take 4 serieThueMorse
--    [[0],[0,1],[0,1,1,0],[0,1,1,0,1,0,0,1]]
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module La_serie_de_Thue_Morse where

import Test.QuickCheck (NonNegative (NonNegative), quickCheckWith, maxSize, stdArgs)

-- 1ª solución
-- ===========

serieThueMorse1 :: [[Int]]
serieThueMorse1 = map termSerieThueMorse [0..]

-- (termSerieThueMorse n) es el término n-ésimo de la serie de
-- Thue-Morse. Por ejemplo, 
--    termSerieThueMorse 1  ==  [0,1]
--    termSerieThueMorse 2  ==  [0,1,1,0]
--    termSerieThueMorse 3  ==  [0,1,1,0,1,0,0,1]
--    termSerieThueMorse 4  ==  [0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0]
termSerieThueMorse :: Int -> [Int]
termSerieThueMorse 0 = [0]
termSerieThueMorse n = xs ++ complementaria xs
  where xs = termSerieThueMorse (n-1)

-- (complementaria xs) es la complementaria de la lista xs (formada por
-- ceros y unos); es decir, la lista obtenida cambiando el 0 por 1 y el
-- 1 por 0. Por ejemplo, 
--    complementaria [1,0,0,1,1,0,1]  ==  [0,1,1,0,0,1,0]
complementaria :: [Int] -> [Int]
complementaria = map (1-)

-- 2ª solución
-- ===========

serieThueMorse2 :: [[Int]]
serieThueMorse2 = [0] : map paso serieThueMorse2
  where paso xs = xs ++ complementaria xs

-- 3ª solución
-- ===========

serieThueMorse3 :: [[Int]]
serieThueMorse3 = iterate paso [0]
  where paso xs = xs ++ complementaria xs

-- 4ª solución
-- ===========

-- Observando que cada término de la serie de Thue-Morse se obtiene del
-- anterior sustituyendo los 1 por 1, 0 y los 0 por  0, 1. 

serieThueMorse4 :: [[Int]]
serieThueMorse4 = [0] : map (concatMap paso4) serieThueMorse4
  where paso4 x = [x,1-x]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_serieThueMorse :: NonNegative Int -> Bool 
prop_serieThueMorse  (NonNegative n) =
  all (== serieThueMorse1 !! n)
      [serieThueMorse2 !! n,
       serieThueMorse3 !! n,
       serieThueMorse4 !! n]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=20}) prop_serieThueMorse
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> (serieThueMorse1 !! 23) !! (2^22)
--    1
--    (1.08 secs, 839,419,224 bytes)
--    λ> (serieThueMorse2 !! 23) !! (2^22)
--    1
--    (0.61 secs, 839,413,592 bytes)
--    λ> (serieThueMorse3 !! 23) !! (2^22)
--    1
--    (1.43 secs, 839,413,592 bytes)
--    λ> (serieThueMorse4 !! 23) !! (2^22)
--    1
--    (1.57 secs, 1,007,190,024 bytes)

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- + N.J.A. Sloane "Sucesión A010060" en OEIS http://oeis.org/A010060
-- + Programming Praxis "Thue-Morse sequence" http://bit.ly/1n2PdFk
-- + Wikipedia "Thue–Morse sequence" http://bit.ly/1KvZONW

