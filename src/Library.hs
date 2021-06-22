module Library where
import PdePreludat

-- Punto 1.a
data Pais = Pais {
    ingresoPerCapita        :: Number,
    poblacionActivaPublico  :: Number,
    poblacionActivaPrivado  :: Number,
    recursosNaturales       :: [String],
    deuda                   :: Number
}

-- Punto 1.b
namibia :: Pais
namibia = Pais 4140 400000 650000 ["Mineria", "Ecoturismo"] 50

-- Punto 2
type Estrategia = Pais -> Pais

prestamo :: Number -> Estrategia
prestamo valor pais = pais {
    deuda = deuda pais + valor * 1.5
}

reducirPuestos :: Number -> Estrategia
reducirPuestos puestosDeTrabajo pais = pais {
    poblacionActivaPublico = poblacionActivaPublico pais - puestosDeTrabajo,
    ingresoPerCapita = ingresoPerCapita pais * (1 - reduccionTrabajo puestosDeTrabajo)
}

reduccionTrabajo :: Number -> Number
reduccionTrabajo puestosDeTrabajo 
 | puestosDeTrabajo > 100 = 0.2
 | otherwise              = 0.15

explotacion :: String -> Estrategia
explotacion recurso = explotarRecurso recurso . disminuirDeuda 2

explotarRecurso :: String -> Estrategia
explotarRecurso recurso pais = pais {
    recursosNaturales = filter (/= recurso) . recursosNaturales $ pais 
}

disminuirDeuda :: Number -> Estrategia
disminuirDeuda valor pais = pais {
    deuda = deuda pais - valor
}

blindaje :: Estrategia
blindaje pais = reducirPuestos 500 . prestamo (pbi pais * 0.5) $ pais 

pbi :: Pais -> Number
pbi pais = ingresoPerCapita pais * poblacionActiva pais

poblacionActiva :: Pais -> Number
poblacionActiva pais = poblacionActivaPrivado pais + poblacionActivaPublico pais

-- Punto 3.a
type Receta = [Estrategia]

unaReceta :: Receta
unaReceta = [prestamo 200, explotacion "Mineria"]

-- Punto 3.b
aplicarReceta :: Pais -> Receta -> Pais
aplicarReceta pais recetas = foldr ($) pais recetas

-- Punto 4.a
zafar :: [Pais] -> [Pais]
zafar = filter $ (elem "Petroleo") . recursosNaturales

-- Punto 4.b
deudaTotal :: [Pais] -> Number
deudaTotal = sum . map deuda

--Punto 5
paisDePeorAMejor :: Pais -> [Receta] -> Bool
paisDePeorAMejor _ [receta] = True
paisDePeorAMejor pais (primeraReceta:segundaReceta:restoRecetas) 
    = revisarPBI pais primeraReceta  <= revisarPBI pais segundaReceta && paisDePeorAMejor pais (segundaReceta:restoRecetas)

revisarPBI :: Pais -> Receta -> Number
revisarPBI pais = (pbi . aplicarReceta pais)

-- Punto 6
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

pruebaInfinita1 = zafar [namibia, Pais 1 1 1 recursosNaturalesInfinitos 1]
--              no termina nunca, porque quiere buscar "Mineria" entre los recursos
pruebaInfinita2 = deudaTotal [namibia, Pais 1 1 1 recursosNaturalesInfinitos 1]
--              se puede porque al no evaluar los recursos solamente suma deuda
-- relacionado con evaluacion diferida, solo se evalua lo que se necesita

