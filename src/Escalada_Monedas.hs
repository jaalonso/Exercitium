-- Escalada_Monedas.hs
-- Problema de las monedas por búsqueda en escalada.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 4-agosto-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- El problema del cambio de monedas consiste en determinar cómo
-- conseguir una cantidad usando el menor número de monedas
-- disponibles. Se supone que se posee un número ilimitado de monedas de
-- 1, 2, 5, 10, 20, 50 y 100 euros. Por ejemplo, para conseguir 199 se
-- necesitan como mínimo 7 monedas (129 = 2 + 2 + 5 + 20 + 20 + 50 + 100).
--
-- En la representación se usarán los siguientes tipos:
-- + Moneda, que es un número entero representado el valor de la moneda
-- + Soluciones, que es una lista de monedas cuya suma es la cantidad
--   deseada y no nay ninguna lista más corta con la misma suma.
--
-- Usando la [búsqueda en escalada](https://bit.ly/3Kk4A99), definir la
-- función
--    cambio :: Int -> Soluciones
-- tal que (cambio n) es la solución del problema de las monedas, para
-- obtener la cantidad n, por búsqueda en escalada. Por ejemplo,
--    cambio 199  ==  [2,2,5,20,20,50,100]
-- ---------------------------------------------------------------------

module Escalada_Monedas where

import BusquedaEnEscalada
import Test.Hspec (Spec, hspec, it, shouldBe)

-- Las monedas son números enteros.
type Moneda = Int

-- monedas es la lista del tipo de monedas disponibles. Se supone que
-- hay un número infinito de monedas de cada tipo.
monedas :: [Moneda]
monedas = [1,2,5,10,20,50,100]

-- Las soluciones son listas de monedas.
type Soluciones = [Moneda]

-- Los estados son pares formados por la cantidad que falta y la lista
-- de monedas usadas.
type Estado = (Int, [Moneda])

-- (inicial n) es el estado inicial del problema de las monedas, para
-- obtener la cantidad n.
inicial :: Int -> Estado
inicial n = (n, [])

-- (esFinal e) se verifica si e es un estado final del problema
-- de las monedas.
esFinal :: Estado -> Bool
esFinal (v,_) = v == 0

-- (sucesores e) es la lista de los sucesores del estado e en el
-- problema de las monedas. Por ejemplo,
--   λ> sucesores (199,[])
--   [(198,[1]),(197,[2]),(194,[5]),(189,[10]),
--    (179,[20]),(149,[50]),(99,[100])]
sucesores :: Estado -> [Estado]
sucesores (r,p) =
  [(r-c,c:p) | c <- monedas, r-c >= 0]

cambio :: Int -> Soluciones
cambio n =
  snd (head (buscaEscalada sucesores
                           esFinal
                           (inicial n)))
-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    cambio 199  `shouldBe`  [2,2,5,20,20,50,100]

-- La verificación es
--    λ> verifica
--
--    e1
--
--    Finished in 0.0003 seconds
--    1 example, 0 failures
