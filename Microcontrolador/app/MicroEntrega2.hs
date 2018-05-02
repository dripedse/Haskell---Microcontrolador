module MicroEntrega2 where
-- Punto 1:

-- Se utiliza Record Syntax dado que evita definiciones reduntantes y suma expresividad al modelado de la informacion. 
-- Se tiene a ventaja de poder alterar el orden en que se definen los valores y se suman funciones adquiridas que permiten 
-- preguntar por las propiedades del dato. Ademas al mostrar el dato por consola, la informacion se muestra ordenada y con 
-- su categoria.

--Para solucionar error no instance for Show
import Text.Show.Functions()

type Instruccion = Microprocesador -> Microprocesador
type Programa = [Instruccion]

data Microprocesador = Microprocesador {
    memoria :: [Int],
    acumuladorA :: Int,
    acumuladorB :: Int,
    programCounter :: Int,
    mensajeError :: String,
    programa :: Programa
} deriving (Show)


xt8088 :: Microprocesador
xt8088 = Microprocesador {
    memoria=replicate 1024 0,
    acumuladorA=0,
    acumuladorB=0,
    programCounter=0,
    mensajeError="",
    programa = vacio
   
}


aumentarPC :: Instruccion
aumentarPC microprocesador = microprocesador { programCounter=programCounter microprocesador + 1}

ejecutar :: Instruccion -> Microprocesador -> Microprocesador
ejecutar instruccion  = aumentarPC.instruccion

ejecutarMuchas :: [Instruccion] -> Microprocesador -> Microprocesador
ejecutarMuchas instrucciones micro = foldr ejecutar micro instrucciones


-- Punto 2:

nop :: Instruccion
nop microprocesador = microprocesador 


-- Punto 3

lodv :: Int -> Instruccion
lodv value microprocesador = microprocesador { acumuladorA = value}

swap :: Instruccion
swap microprocesador = microprocesador { acumuladorA = acumuladorB microprocesador, acumuladorB = acumuladorA microprocesador }

add :: Instruccion
add microprocesador = microprocesador { acumuladorA = acumuladorA microprocesador + acumuladorB microprocesador, acumuladorB = 0}




-- Punto 4

divide :: Instruccion
divide microprocesador | acumuladorB microprocesador /= 0 = microprocesador { acumuladorA = div (acumuladorA microprocesador) (acumuladorB microprocesador), acumuladorB = 0}
                       | otherwise = microprocesador { mensajeError = "DIVISION BY ZERO"}

    

str :: Int -> Int -> Instruccion
str address value microprocesador= microprocesador { memoria = agregarDato (memoria microprocesador) address value}
agregarDato :: [a] -> Int -> a -> [a]
agregarDato memory address value = take (address-1) memory ++ [value] ++ drop (address) memory

lod :: Int -> Instruccion
lod address microprocesador = microprocesador {  acumuladorA= cargarAcumulador address (memoria microprocesador)}
cargarAcumulador :: Int -> [Int] -> Int
cargarAcumulador address memory = memory !! (address-1)

-- Punto 5

cargarPrograma :: Programa -> Microprocesador -> Microprocesador
cargarPrograma programaNuevo microprocesador = microprocesador { programa = programaNuevo}

suma10y22 :: Programa
suma10y22 = [add, lodv 22, swap, lodv 10]

division2por0 :: Programa 
division2por0 = [divide, lod 1, swap, lod 2, str 2 0, str 1 2]

vacio :: Programa
vacio = []
 

-- Punto 6

ejecutarPrograma :: Microprocesador -> Microprocesador
ejecutarPrograma micro = ejecutarMuchas (programa micro) micro


-- Punto  7

ifNZ :: Programa -> Microprocesador -> Microprocesador
ifNZ instrucciones micro | (acumuladorA micro) /= 0 = ejecutarMuchas instrucciones micro
                         | otherwise = micro

-- punto 8

depurar :: Microprocesador -> [Instruccion] -> [Instruccion]
depurar micro programaTest = filter (instruccionOk micro) programaTest

instruccionOk :: Microprocesador -> Instruccion -> Bool
instruccionOk microTest instruccion = (acumuladorA.ejecutar instruccion) microTest /= 0 && (acumuladorB.ejecutar instruccion) microTest /= 0 && not (memoriaCero  microTest instruccion)

memoriaCero :: Microprocesador -> Instruccion -> Bool
memoriaCero micro instruccion = all (==0) ((memoria.ejecutar instruccion) micro)






















-- CASOS DE PRUEBA: 

-- Punto 2:
-- *MicroEntrega1> (nop.nop.nop) xt8088
--  Microprocesador {memoria = [], acumulador1 = 0, acumulador2 = 0, programCounter = 3, mensajeError = ""}

-- Punto 3:

-- *MicroEntrega1> lodv 5 xt8088
--  Microprocesador {memoria = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], acumuladorA = 5, acumuladorB = 0, programCounter = 1, etiqueta = ""}

fp20 :: Microprocesador
fp20 = Microprocesador {
    memoria=[0],
    acumuladorA=7,
    acumuladorB=24,
    programCounter=0,
    mensajeError="",
    programa = vacio
}
-- *MicroEntrega1> swap fp20
-- Microprocesador {memoria = [0], acumuladorA = 24, acumuladorB = 7, programCounter = 1, mensajeError = ""}

-- *MicroEntrega1> (add.(lodv 22).swap.(lodv 10)) xt8088
--  Microprocesador {memoria = [], acumuladorA = 32, acumuladorB = 0, programCounter = 4, mensajeError = ""}

-- Punto 4: 
at8086 :: Microprocesador
at8086 = Microprocesador {
    memoria=[1..20],
    acumuladorA=0,
    acumuladorB=0,
    programCounter=0,
    mensajeError="",
    programa = vacio
}

-- *MicroEntrega1> str 2 5 at8086
-- Microprocesador {memoria = [1,5,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20], acumuladorA = 0, acumuladorB = 0, programCounter = 1, mensajeError = ""}

-- *MicroEntrega1> lod 2 xt8088
-- Microprocesador {memoria = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], acumuladorA = 0, acumuladorB = 0, programCounter = 1, mensajeError = ""}

-- *MicroEntrega1> (divide.(lod 1) . swap . (lod 2) . (str 2 0) . (str 1 2)) xt8088
--  Microprocesador {memoria = [2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], acumuladorA = 2, acumuladorB = 0, programCounter = 6, mensajeError = "DIVISION BY ZERO"}

-- *MicroEntrega1> (divide.(lod 1) . swap . (lod 2) . (str 2 4) . (str 1 12)) xt8088
-- Microprocesador {memoria = [12,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], acumuladorA = 3, acumuladorB = 0, programCounter = 6, mensajeError = ""}

