-- Movimientos_en_el_plano.hs
-- Movimientos en el plano.
-- José A. Alonso Jiménez <https://jaalonso.github.io>
-- Sevilla, 23-noviembre-2022
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Se consideran el tipo de las posiciones del plano definido por
--    type Posicion = (Int,Int)
-- y el tipo de las direcciones definido por
--    data Direccion = Izquierda | Derecha | Arriba | Abajo
--      deriving Show
--
-- Definir las siguientes funciones
--    opuesta     :: Direccion -> Direccion
--    movimiento  :: Posicion -> Direccion -> Posicion
--    movimientos :: Posicion -> [Direccion] -> Posicion
-- tales que
-- + (opuesta d) es la dirección opuesta de d. Por ejemplo,
--      opuesta Izquierda == Derecha
-- + (movimiento p d) es la posición reultante de moverse, desde la
--   posición p, un paso en la dirección d . Por ejemplo,
--      movimiento (2,5) Arriba          == (2,6)
--      movimiento (2,5) (opuesta Abajo) == (2,6)
-- + (movimientos p ds) es la posición obtenida aplicando la lista de
--   movimientos según las direcciones de ds a la posición p. Por ejemplo,
--      movimientos (2,5)  [Arriba, Izquierda] == (1,6)
-- ---------------------------------------------------------------------

module El_tipo_de_los_movimientos where

type Posicion = (Int,Int)

data Direccion = Izquierda | Derecha | Arriba | Abajo
  deriving Show

-- Definición de opuesta
-- =====================

opuesta :: Direccion -> Direccion
opuesta Izquierda = Derecha
opuesta Derecha   = Izquierda
opuesta Arriba    = Abajo
opuesta Abajo     = Arriba

-- 1ª definición de movimiento
-- ===========================

movimiento1 :: Posicion -> Direccion -> Posicion
movimiento1 (x,y) Izquierda = (x-1,y)
movimiento1 (x,y) Derecha   = (x+1,y)
movimiento1 (x,y) Arriba    = (x,y+1)
movimiento1 (x,y) Abajo     = (x,y-1)

-- 2ª definición de movimiento
-- ===========================

movimiento2 :: Posicion -> Direccion -> Posicion
movimiento2 (x,y) d =
  case d of
    Izquierda -> (x-1,y)
    Derecha   -> (x+1,y)
    Arriba    -> (x,y+1)
    Abajo     -> (x,y-1)

-- 1ª definición de movimientos
-- ============================

movimientos1 :: Posicion -> [Direccion] -> Posicion
movimientos1 p []     = p
movimientos1 p (d:ds) = movimientos1 (movimiento1 p d) ds

-- 2ª definición de movimientos
-- ============================

movimientos2 :: Posicion -> [Direccion] -> Posicion
movimientos2 = foldl movimiento1
