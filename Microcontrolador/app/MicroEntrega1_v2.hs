module MicroEntrega1 where
-- Punto 1:

-- Se utiliza Record Syntax dado que evita definiciones reduntantes y suma expresividad al modelado de la informacion. 
-- Se tiene a ventaja de poder alterar el orden en que se definen los valores y se suman funciones adquiridas que permiten 
-- preguntar por las propiedades del dato. Ademas al mostrar el dato por consola, la informacion se muestra ordenada y con 
-- su categoria.

--Para solucionar error no instance for Show
import Text.Show.Functions()

data Microprocesador = Microprocesador {
    memoria :: [Int],
    acumuladorA :: Int,
    acumuladorB :: Int,
    programCounter :: Int,
    etiqueta :: String
} deriving (Show)

xt8088 :: Microprocesador
xt8088 = Microprocesador {
    memoria=[],
    acumuladorA=0,
    acumuladorB=0,
    programCounter=0,
    etiqueta=""
}

-- Punto 2:



nop :: Microprocesador -> Microprocesador
nop microprocesador = microprocesador { programCounter=programCounter microprocesador + 1}

-- En consola:
-- *MicroEntrega1> (nop.nop.nop) xt8088
-- Microprocesador {memoria = [], acumulador1 = 0, acumulador2 = 0, programCounter = 3, etiqueta = ""}
-- Se uso el concepto de composicion de funciones

-- Punto 3

lodv :: Int -> Microprocesador -> Microprocesador
lodv value microprocesador = microprocesador { acumuladorA = value, programCounter = programCounter microprocesador + 1}
--lodv valor microprocesador = microprocesador { acumuladorA = valor }

swap :: Microprocesador -> Microprocesador
swap microprocesador = microprocesador { acumuladorA = acumuladorB microprocesador, acumuladorB = acumuladorA microprocesador, programCounter = programCounter microprocesador + 1 }
-- swap microprocesador = microprocesador { acumuladorA = acumuladorB microprocesador, acumuladorB = acumuladorA microprocesador 1 }

add :: Microprocesador -> Microprocesador
add microprocesador = microprocesador { acumuladorA = acumuladorA microprocesador + acumuladorB microprocesador, acumuladorB = 0, programCounter = programCounter microprocesador + 1}
-- add microprocesador = microprocesador { acumuladorA = acumuladorA microprocesador + acumuladorB microprocesador, acumuladorB = 0}
-- *MicroEntrega1> (nop.add.nop.(lodv 22).nop.swap.nop.(lodv 10)) xt8088
--  Microprocesador {memoria = [], acumuladorA = 32, acumuladorB = 0, programCounter = 4, etiqueta = ""}

-- *MicroEntrega1> (add.(lodv 22).swap.(lodv 10)) xt8088
--  Microprocesador {memoria = [], acumuladorA = 32, acumuladorB = 0, programCounter = 4, etiqueta = ""}


-- Punto 4

divide :: Microprocesador -> Microprocesador
divide microprocesador | acumuladorB microprocesador /= 0 = microprocesador { acumuladorA = div (acumuladorA microprocesador) (acumuladorB microprocesador), acumuladorB = 0, programCounter = programCounter microprocesador + 1}
                       | otherwise = microprocesador { etiqueta = "DIVISION BY ZERO", programCounter = programCounter microprocesador + 1}

-- str address value microprocesador = microprocesador { memoria = }                        