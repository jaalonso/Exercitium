-- Huecos_maximales_entre_primos.hs
-- Huecos maximales entre primos.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 25-julio-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El **hueco de un número primo** p es la distancia entre p y primo
-- siguiente de p. Por ejemplo, el hueco de 7 es 4 porque el primo
-- siguiente de 7 es 11 y 4 = 11-7. Los huecos de los primeros números son
--    Primo Hueco
--     2    1
--     3    2
--     7    4
--    11    2
-- 
-- El hueco de un número primo p es **maximal** si es mayor que los
-- huecos de todos los números menores que p. Por ejemplo, 4 es un hueco
-- maximal de 7 ya que los huecos de los primos menores que 7 son 1 y 2
-- y ambos son menores que 4. La tabla de los primeros huecos maximales es
--    Primo Hueco
--      2    1
--      3    2
--      7    4
--     23    6
--     89    8
--    113   14
--    523   18
--    887   20
-- 
-- Definir la sucesión
--    primosYhuecosMaximales :: [(Integer,Integer)]
-- cuyos elementos son los números primos con huecos maximales junto son
-- sus huecos. Por ejemplo,
--    λ> take 8 primosYhuecosMaximales
--    [(2,1),(3,2),(7,4),(23,6),(89,8),(113,14),(523,18),(887,20)]
--    λ> primosYhuecosMaximales !! 20
--    (2010733,148)
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Huecos_maximales_entre_primos where

import Data.Numbers.Primes (primes)
import Test.QuickCheck (NonNegative (NonNegative), quickCheckWith, maxSize, stdArgs)

-- 1ª solución
-- ===========

primosYhuecosMaximales1 :: [(Integer,Integer)]
primosYhuecosMaximales1 = 
  [(p,huecoPrimo p) | p <- primes, esMaximalHuecoPrimo p]

-- (siguientePrimo x) es el menor primo mayor que x. Por ejemplo,
--    siguientePrimo 7  ==  11
--    siguientePrimo 8  ==  11
siguientePrimo :: Integer -> Integer
siguientePrimo p =
  head (dropWhile (<= p) primes)

-- (huecoPrimo p) es la distancia del primo p hasta el siguiente
-- primo. Por ejemplo,
--    huecoPrimo 7  ==  4
huecoPrimo :: Integer -> Integer
huecoPrimo p = siguientePrimo p - p

-- (esMaximalHuecoPrimo p) se verifica si el hueco primo de p es
-- maximal. Por ejemplo,
--    esMaximalHuecoPrimo  7  ==  True
--    esMaximalHuecoPrimo 11  ==  False
esMaximalHuecoPrimo :: Integer -> Bool
esMaximalHuecoPrimo p =
  and [huecoPrimo n < h | n <- takeWhile (< p) primes]
  where h = huecoPrimo p

-- 2ª solución
-- ===========

primosYhuecosMaximales2 :: [(Integer,Integer)]
primosYhuecosMaximales2 = aux primosYhuecos
  where aux ((x,y):ps) = (x,y) : aux (dropWhile (\(_,b) -> b <= y) ps)

-- primosYhuecos es la lista de los números primos junto son sus
-- huecos. Por ejemplo, 
--    λ> take 10 primosYhuecos
--    [(2,1),(3,2),(5,2),(7,4),(11,2),(13,4),(17,2),(19,4),(23,6),(29,2)]
primosYhuecos :: [(Integer,Integer)]
primosYhuecos =
  [(x,y-x) | (x,y) <- zip primes (tail primes)]

-- 3ª solución
-- ===========

primosYhuecosMaximales3 :: [(Integer,Integer)]
primosYhuecosMaximales3 = aux 0 primes
  where aux n (x:y:zs) | y-x > n   = (x,y-x) : aux (y-x) (y:zs)
                       | otherwise = aux n (y:zs)

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_primosYhuecosMaximales :: NonNegative Int -> Bool
prop_primosYhuecosMaximales (NonNegative n) =
  all (== primosYhuecosMaximales1 !! n)
      [primosYhuecosMaximales2 !! n,
       primosYhuecosMaximales3 !! n]

-- La comprobación es
--    λ> quickCheckWith (stdArgs {maxSize=12}) prop_primosYhuecosMaximales
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> primosYhuecosMaximales1 !! 10
--    (9551,36)
--    (2.63 secs, 7,400,316,112 bytes)
--    λ> primosYhuecosMaximales2 !! 10
--    (9551,36)
--    (0.01 secs, 7,060,744 bytes)
--    λ> primosYhuecosMaximales3 !! 10
--    (9551,36)
--    (0.01 secs, 4,000,368 bytes)
--    
--    λ> primosYhuecosMaximales2 !! 22
--    (17051707,180)
--    (7.90 secs, 17,275,407,712 bytes)
--    λ> primosYhuecosMaximales3 !! 22
--    (17051707,180)
--    (3.78 secs, 8,808,779,096 bytes)

-- ---------------------------------------------------------------------
-- § Referencias                                                      --
-- ---------------------------------------------------------------------

-- Basado en el ejercicio "Maximal prime gaps" http://bit.ly/22UfDJN de
-- Programming Praxis http://programmingpraxis.com

-- Otras referencias
-- + C. Caldwell, "The gaps between primes" http://bit.ly/1Znusp5
-- + J.K. Andersen, "Maximal prime gaps" http://bit.ly/1ZntwRi
-- + N.J.A. Sloane "Sequence A002386" en OEIS http://oeis.org/A002386
-- + N.J.A. Sloane "Sequence A005250" en OEIS http://oeis.org/A005250
-- + E.W. Weisstein, "Prime gaps" en MathWorld http://bit.ly/1ZnubCq
