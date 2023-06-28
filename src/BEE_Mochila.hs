-- BEE_Mochila.hs
-- El problema de la mochila (mediante espacio de estados).
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 04-julio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se tiene una mochila de capacidad de peso p y una lista de n objetos
-- para colocar en la mochila. Cada objeto i tiene un peso w(i) y un
-- valor v(i). Considerando la posibilidad de colocar el mismo objeto
-- varias veces en la mochila, el problema consiste en determinar la
-- forma de colocar los objetos en la mochila sin sobrepasar la
-- capacidad de la mochila colocando el máximo valor posible.
--
-- Para solucionar el problema se definen los siguientes tipos:
-- + Una solución del problema de la mochila es una lista de objetos.
--      type SolMoch = [Objeto]
-- + Los objetos son pares formado por un peso y un valor
--      type Objeto = (Peso,Valor)
-- + Los pesos son número enteros
--      type Peso = Int
-- + Los valores son números reales.
--      type Valor = Float
-- + Los estados del problema de la mochila son 5-tupla de la forma
--   (v,p,l,o,s) donde v es el valor de los objetos colocados, p es el
--   peso de los objetos colocados, l es el límite de la capacidad de la
--   mochila, o es la lista de los objetos colocados (ordenados de forma
--   creciente según sus pesos) y s es la solución parcial.
--      type NodoMoch = (Valor,Peso,Peso,[Objeto],SolMoch)
--
-- Usando el procedimiento de [búsqueda en profundidad](http://bit.ly/2sqPtGs),
-- definir la función
--    mochila :: [Objeto] -> Peso -> (SolMoch,Valor)
-- tal que (mochila os l) es la solución del problema de la mochila para
-- la lista de objetos os y el límite de capacidad l. Por ejemplo,
--    > mochila [(2,3),(3,5),(4,6),(5,10)] 8
--    ([(5,10.0),(3,5.0)],15.0)
--    > mochila [(2,3),(3,5),(5,6)] 10
--    ([(3,5.0),(3,5.0),(2,3.0),(2,3.0)],16.0)
--    > mochila [(8,15),(15,10),(3,6),(6,13),(2,4),(4,8),(5,6),(7,7)] 35
--    ([(6,13.0),(6,13.0),(6,13.0),(6,13.0),(6,13.0),(3,6.0),(2,4.0)],75.0)
--    > mochila [(2,2.8),(3,4.4),(5,6.1)] 10
--    ([(3,4.4),(3,4.4),(2,2.8),(2,2.8)],14.4)
-- ---------------------------------------------------------------------

module BEE_Mochila where

import BusquedaEnProfundidad (buscaProfundidad)
import Data.List (sort)
import Test.Hspec (Spec, hspec, it, shouldBe)

type Peso     = Int
type Valor    = Float
type Objeto   = (Peso,Valor)
type SolMoch  = [Objeto]
type NodoMoch = (Valor,Peso,Peso,[Objeto],SolMoch)

mochila :: [Objeto] -> Peso -> (SolMoch,Valor)
mochila os limite = (sol,v)
  where
    (v,_,_,_,sol) =
      maximum (buscaProfundidad sucesoresMoch
                                esObjetivoMoch
                                (inicial os limite))

-- (inicial os limite) es el estado inicial del problema de la mochila
-- para la lista de objetos os y el límite de capacidad l
inicial :: [Objeto] -> Peso -> NodoMoch
inicial os l =
  (0,0,l,sort os,[])

-- (sucesoresMoch e) es la lista de los sucesores del estado e en el
-- problema de la mochila para la lista de objetos os y el límite de
-- capacidad l.
sucesoresMoch :: NodoMoch -> [NodoMoch]
sucesoresMoch (v,p,l,os,solp) =
  [( v+v',
     p+p',
     l,
     [o | o@(p'',_) <- os, p''>=p'],
     (p',v'):solp )
  | (p',v') <- os,
    p+p' <= l]

-- (esObjetivoMoch e) se verifica si e es un estado final el problema de
-- la mochila para la lista de objetos os y el límite de capacidad l .
esObjetivoMoch :: NodoMoch -> Bool
esObjetivoMoch (_,p,l,(p',_):_,_) = p+p'>l
esObjetivoMoch _ = error "Imposible"

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    mochila [(2,3),(3,5),(4,6),(5,10)] 8
    `shouldBe` ([(5,10.0),(3,5.0)],15.0)
  it "e2" $
    mochila [(2,3),(3,5),(5,6)] 10
    `shouldBe` ([(3,5.0),(3,5.0),(2,3.0),(2,3.0)],16.0)
  it "e3" $
    mochila [(8,15),(15,10),(3,6),(6,13),(2,4),(4,8),(5,6),(7,7)] 35
    `shouldBe` ([(6,13.0),(6,13.0),(6,13.0),(6,13.0),(6,13.0),(3,6.0),(2,4.0)],75.0)
  it "e4" $
    mochila [(2,2.8),(3,4.4),(5,6.1)] 10
    `shouldBe` ([(3,4.4),(3,4.4),(2,2.8),(2,2.8)],14.4)

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--    e4
--
--    Finished in 0.0424 seconds
--    4 examples, 0 failures
