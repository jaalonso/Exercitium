-- BusquedaEnProfundidad.hs
-- Búsqueda en espacios de estados por profundidad.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 28-junio-2023
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Las características de los problemas de espacios de estados son:
-- + un conjunto de las posibles situaciones o nodos que constituye el
--   espacio de estados (estos son las potenciales soluciones que se
--   necesitan explorar),
-- + un conjunto de movimientos de un nodo a otros nodos, llamados los
--   sucesores del nodo,
-- + un nodo inicial y
-- + un nodo objetivo que es la solución.
--
-- Definir la función
--    buscaProfundidad :: Eq nodo => (nodo -> [nodo]) -> (nodo -> Bool)
--                                   -> nodo -> [nodo]
-- tal que (buscaProfundidad s o e) es  la lista de soluciones del
-- problema de espacio de estado definido por la función sucesores s, el
-- objetivo o y estado inicial e obtenidas mediante búsqueda en
-- profundidad.
-- ---------------------------------------------------------------------

module BusquedaEnProfundidad (buscaProfundidad) where

import TAD.Pila

buscaProfundidad :: Eq nodo => (nodo -> [nodo]) -> (nodo -> Bool)
                               -> nodo -> [nodo]
buscaProfundidad sucesores esFinal inicial =
  aux (apila inicial vacia)
  where
    aux p
      | esVacia p        = []
      | esFinal (cima p) = cima p : aux (desapila p)
      | otherwise        = aux (foldr
                                apila
                                (desapila p)
                                (sucesores (cima p)))
