-- BusquedaEnAnchura.hs
-- Búsqueda en espacios de estados por anchura.
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
--    buscaAnchura :: Eq nodo => (nodo -> [nodo]) -> (nodo -> Bool)
--                               -> nodo -> [nodo]
-- tal que (buscaAnchura s o e) es  la lista de soluciones del
-- problema de espacio de estado definido por la función sucesores s, el
-- objetivo o y estado inicial e obtenidas mediante búsqueda en
-- anchura.
-- ---------------------------------------------------------------------

module BusquedaEnAnchura (buscaAnchura) where

import TAD.Cola (esVacia, inserta, primero, resto, vacia)

buscaAnchura :: Eq nodo => (nodo -> [nodo]) -> (nodo -> Bool)
                               -> nodo -> [nodo]
buscaAnchura sucesores esFinal inicial =
  aux (inserta inicial vacia)
  where
    aux p
      | esVacia p        = []
      | esFinal (primero p) = primero p : aux (resto p)
      | otherwise        = aux (foldr
                                inserta
                                (resto p)
                                (sucesores (primero p)))
