-- Problema_de_las_jarras.hs
-- Problema de las jarras.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 9-septiembre-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- En el problema de las jarras (A,B,C) se tienen dos jarras sin marcas
-- de medición, una de A litros de capacidad y otra de B. También se
-- dispone de una bomba que permite llenar las jarras de agua.
--
-- El problema de las jarras (A,B,C) consiste en determinar cómo se
-- puede lograr tener exactamente C litros de agua en la jarra de A
-- litros de capacidad.
--
-- Usando el [procedimiento de búsqueda en anchura](https://bit.ly/3XBlqG7),
-- definir la función
--    jarras :: (Int,Int,Int) -> [[(Int,Int)]]
-- tal (jarras (a,b,c)) es la lista de las soluciones del problema de las
-- jarras (a,b,c). Por ejemplo,
--    λ> take 3 (jarras (4,3,2))
--    [[(0,0),(0,3),(3,0),(3,3),(4,2),(0,2),(2,0)],
--     [(0,0),(4,0),(1,3),(1,0),(0,1),(4,1),(2,3)],
--     [(0,0),(0,3),(3,0),(4,0),(1,3),(1,0),(0,1),(4,1),(2,3)]]
--
-- La interpretación [(0,0),(4,0),(1,3),(1,0),(0,1),(4,1),(2,3)] es:
--    (0,0) se inicia con las dos jarras vacías,
--    (4,0) se llena la jarra de 4 con el grifo,
--    (1,3) se llena la de 3 con la de 4,
--    (1,0) se vacía la de 3,
--    (0,1) se pasa el contenido de la primera a la segunda,
--    (4,1) se llena la primera con el grifo,
--    (2,3) se llena la segunda con la primera.
--
-- Otros ejemplos
--    λ> length (jarras (15,10,5))
--    8
--    λ> map length (jarras (15,10,5))
--    [3,5,5,7,7,7,8,9]
--    λ> jarras (15,10,4)
--    []
-- ---------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Problema_de_las_jarras where

import BusquedaEnAnchura (buscaAnchura)
import Test.Hspec (Spec, hspec, it, shouldBe)

-- Un problema es una lista de 3 números enteros (a,b,c) tales que a es
-- la capacidad de la primera jarra, b es la capacidad de la segunda
-- jarra y c es el número de litros que se desea obtener en la primera
-- jarra.
type Problema = (Int,Int,Int)

-- Una configuracion es una lista de dos números. El primero es el
-- contenido de la primera jarra y el segundo el de la segunda.
type Configuracion = (Int,Int)

-- Inicialmente, las dos jarras están vacías.
configuracionInicial :: Configuracion
configuracionInicial = (0,0)

-- (esConfiguracionFinal p e) se verifica si e es un configuracion final
-- del problema p.
esConfiguracionFinal :: Problema -> Configuracion -> Bool
esConfiguracionFinal (_,_,c) (x,_) = x == c

-- (sucesorasConfiguracion p c) son las sucesoras de la configuración c
-- del problema p. Por ejemplo,
--    sucesorasConfiguracion (4,3,2) (0,0)  ==  [(4,0),(0,3)]
--    sucesorasConfiguracion (4,3,2) (4,0)  ==  [(4,3),(0,0),(1,3)]
--    sucesorasConfiguracion (4,3,2) (4,3)  ==  [(0,3),(4,0)]
sucesorasConfiguracion :: Problema -> Configuracion -> [Configuracion]
sucesorasConfiguracion (a,b,_) (x,y) =
    [(a,y) | x < a] ++
    [(x,b) | y < b] ++
    [(0,y) | x > 0] ++
    [(x,0) | y > 0] ++
    [(a,y-(a-x)) | x < a, y > 0, x + y > a] ++
    [(x-(b-y),b) | x > 0, y < b, x + y > b] ++
    [(x+y,0) | y > 0, x + y <= a] ++
    [(0,x+y) | x > 0, x + y <= b]

-- Los estados son listas de configuraciones [c_n,...c_2,c_1] tales que
-- c_1 es la configuración inicial y, para 2 <= i <= n, c_i es una
-- sucesora de c_(i-1).
type Estado = [Configuracion]

-- inicial es el estado cuyo único elemento es la configuración
-- inicial.
inicial :: Estado
inicial = [configuracionInicial]

-- (esFinal p e) se verifica si e es un estado final; es decir, su
-- primer elemento es una configuración final.
esFinal :: Problema -> Estado -> Bool
esFinal p (e:_) = esConfiguracionFinal p e

-- (sucesores p e) es la lista de los sucesores del estado e en el
-- problema p. Por ejemplo,
--    λ> sucesores (4,3,2) [(0,0)]
--    [[(4,0),(0,0)],[(0,3),(0,0)]]
--    λ> sucesores (4,3,2) [(4,0),(0,0)]
--    [[(4,3),(4,0),(0,0)],[(1,3),(4,0),(0,0)]]
--    λ> sucesores (4,3,2) [(4,3),(4,0),(0,0)]
--    [[(0,3),(4,3),(4,0),(0,0)]]
sucesores :: Problema -> Estado -> [Estado]
sucesores p e@(c:_) =
    [c':e | c' <- sucesorasConfiguracion p c,
            c' `notElem` e]

jarras :: Problema -> [Estado]
jarras p = map reverse soluciones
  where
     soluciones = buscaAnchura (sucesores p) (esFinal p) inicial

-- Verificación
-- ============

verifica :: IO ()
verifica = hspec spec

spec :: Spec
spec = do
  it "e1" $
    take 3 (jarras (4,3,2)) `shouldBe`
    [[(0,0),(0,3),(3,0),(3,3),(4,2),(0,2),(2,0)],
     [(0,0),(4,0),(1,3),(1,0),(0,1),(4,1),(2,3)],
     [(0,0),(0,3),(3,0),(4,0),(1,3),(1,0),(0,1),(4,1),(2,3)]]
  it "e2" $
    length (jarras (15,10,5)) `shouldBe` 8
  it "e3" $
    map length (jarras (15,10,5)) `shouldBe`
    [3,5,5,7,7,7,8,9]
  it "e4" $
    jarras (15,10,4) `shouldBe` []

-- La verificación es
--    λ> verifica
--
--    e1
--    e2
--    e3
--    e4
--
--    Finished in 0.0080 seconds
--    4 examples, 0 failures
