# Cuadrado_mas_cercano.py
# Cuadrado más cercano.
# José A. Alonso Jiménez
# Sevilla, 27 de enero de 2022
# ======================================================================

# ---------------------------------------------------------------------
# Definir la función
#    cuadradoCercano :: Integer -> Integer
# tal que (cuadradoCercano n) es el número cuadrado más cercano a n,
# donde n es un entero positivo. Por ejemplo,
#    cuadradoCercano 2       == 1
#    cuadradoCercano 6       == 4
#    cuadradoCercano 8       == 9
#    cuadradoCercano (10^46) == 10000000000000000000000000000000000000000000000
# ---------------------------------------------------------------------

import codewars_test as test


def cuadradoCercano(n):
    return round(n ** 0.5) ** 2


@test.describe("Fixed Tests")
def fixed_tests():
    @test.it('Basic Test Cases')
    def basic_test_cases():
        test.assert_equals(cuadradoCercano(1), 1)
        test.assert_equals(cuadradoCercano(2), 1)
        test.assert_equals(cuadradoCercano(10), 9)
        test.assert_equals(cuadradoCercano(111), 121)
        test.assert_equals(cuadradoCercano(9999), 10000)
