module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

pesoBase :: Number -> Number
pesoBase alturaMetros = 3 * alturaBase (metrosACentimetros alturaMetros)

metrosACentimetros :: Number -> Number
metrosACentimetros metros = metros * 100

alturaLimiteCentimetros :: Number
alturaLimiteCentimetros = 300

alturaBase :: Number -> Number
alturaBase alturaCentimetros = min alturaLimiteCentimetros alturaCentimetros

pesoCopa :: Number -> Number
pesoCopa alturaEnMetros = 2 * alturaCopa (metrosACentimetros alturaEnMetros)

alturaCopa :: Number -> Number
alturaCopa alturaEnCentimetros = max 0 (alturaEnCentimetros - alturaLimiteCentimetros)

pesoPino :: Number -> Number
pesoPino alturaMetros = pesoBase alturaMetros + pesoCopa alturaMetros

type Nombre = String
type Estatura = Number

type Jovit = (Nombre, Estatura)

bilbo :: Jovit
bilbo = ("Bilbo", 125)

rositaCoto :: Jovit
rositaCoto = ("Rosita Coto", 115)

reputacion :: Jovit -> Number
reputacion jovit = longitudNombre jovit * estatura jovit

longitudNombre :: Jovit -> Number
longitudNombre jovit = length (fst jovit)

estatura :: Jovit -> Number
estatura jovit = snd jovit

diferenciaDeAlturas :: Jovit -> Jovit -> Number
diferenciaDeAlturas (_, estatura1) (_, estatura2) = abs (estatura1 - estatura2) 

conNuevaAltura :: Jovit -> Number -> Jovit
conNuevaAltura (nombre, _) estaturaNueva = (nombre, estaturaNueva)

type Fuerza = Number

data Jovit' = UnJovit {
    nombre' :: Nombre,
    estatura' :: Number,
    fuerza :: Fuerza,
    viveEnCumbancha :: Bool
}

bilbo' :: Jovit'
bilbo' = UnJovit "Bilbo" 125 80 True

conNuevaAltura' :: Jovit' -> Number -> Jovit'
conNuevaAltura' jovit nuevaAltura = jovit {estatura' = nuevaAltura}

--}

--viveEnCumbancha bilbo
-- > True