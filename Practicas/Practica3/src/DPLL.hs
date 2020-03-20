{-
- Logica Conmputacional 2020-2 
- Practica 3, Implementación del algoritmo dpll.
- Alumno: Mauricio Riva Palacio Orozco
- Numero de cuenta: 316666343
- Correo: mauricioriva@ciencias.unam.mx
- Alumno: Dicter Tadeo Garcia Rosas
- Numero de cuenta: 316085412
- Correo: dicteraulad@ciencias.unam.mx
-}

module DPLL where

import LProp
import Data.List

type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Solucion = (Modelo, Formula)

-- Seccion de funciones para la regla de la clausula unitaria

unit :: Solucion -> Solucion
unit s@(m,f) = (m ++ k , filter (\x -> x /= k) (f))
        where 
                k = if (p == []) then [] else head p
                p = filter (\x -> length (x) == 1) (f)

--  Seccion de funciones para la regla de eliminacion

elim :: Solucion -> Solucion
elim s@(m,f) = (m , paraCadaLiteral m f)

-- Se le aplica una funcion a cada una de las literales del modelo
paraCadaLiteral :: Modelo -> Formula -> Formula
paraCadaLiteral [] f = f
paraCadaLiteral (x:xs) f = paraCadaLiteral xs (elimAux x f)

-- Devuelve una formula con todas las listas que no contienen a l
elimAux :: Literal -> Formula -> Formula
elimAux _ [] = []
elimAux l f = filter (\x -> notElem l x) f

-- Seccion de funciones para la regla de reduccion

red :: Solucion -> Solucion
red s@(m,f) = (m , paraCadaLiteralRed m f)

-- Se le aplica una funcion a cada una de las literales del modelo
paraCadaLiteralRed :: Modelo -> Formula -> Formula
paraCadaLiteralRed [] f = f
paraCadaLiteralRed (x:xs) f = paraCadaLiteralRed xs (paraCadaCl x f)

-- Se le aplica una funcion a cada una de las clausulas
paraCadaCl :: Literal -> Formula -> Formula
paraCadaCl _ [] = []
paraCadaCl l (x:xs) = [(redAux l x)] ++ paraCadaCl l xs

-- Si el complemento de la literal esta en la clausula, la elimina
redAux :: Literal -> Clausula -> Clausula
redAux _ [] = []
redAux l c
 | (elem k c) = delete k c
 | otherwise = c
        where
                k = meteNeg(Neg l)

-- Seccion de funciones para la regla de separacion

split :: Solucion -> [Solucion]
split s@(m,f)
 | (length literales == 0) = [(m,f)]
 | otherwise = [(literal : m, f), (meteNegAux(literal) : m, f)]
        where simpl = simplifica (m,f)
              literales = listaLiterales m (snd simpl)
              literal = head (head literales)

-- Aplica unit, elim y red a una solucion para simplificarla
simplifica :: Solucion -> Solucion
simplifica s = red (elim (unit s))

-- Te devuelve el complemento de una literal
meteNegAux :: Literal -> Literal
meteNegAux p = meteNeg(Neg p)

-- Te devuelve una lista con todas las literales
listaLiterales :: Modelo -> Formula -> [Modelo]
listaLiterales [] f
 | (length(head f) > 1) = [[head (head f)]]
 | otherwise  = [head f]
listaLiterales x [] = []
listaLiterales (x:xs) (y:ys) = (posibleModelo x y) : listaLiterales xs ys

-- Te devuelve un posible modelo para hacer split
posibleModelo :: Literal -> Clausula -> Modelo
posibleModelo _ [] = []
posibleModelo x (y:ys)
 | x == y = posibleModelo x ys
 | otherwise = y : posibleModelo x ys

-- Seccion de funciones para la regla de conflicto

conflict :: Solucion -> Bool
conflict (m,f) = f == [[]]

-- Seccion de funciones para la regla de exito

success :: Solucion -> Bool
success (m,f) = f == []

-- Ejemplos

exa0 = [[Neg (V "P"), V "R", Neg (V "T")],[Neg (V "Q"), Neg (V "R")],[V "P",Neg (V "S")],[Neg (V "P"), V "Q", Neg (V "R"), Neg (V "S")]]
exa1 = [[V "p", V "q"],[Neg (V "q")],[Neg (V "p"), V "q", Neg (V "r")]]
exa2 = [[V "p", V "q"],[V "p", Neg (V "q")],[V "r", V "q"],[V "r", Neg (V "q")]]    
exa3 = [[V "p", Neg (V "q")],[Neg (V "p"), V "q"],[V "q", Neg (V "r")],[Neg (V "q"), Neg (V "r")]]
exa4 = [[V "p", V "q"], [V "r", Neg (V "q"), Neg (V "s")], [Neg (V "p"), V "s"], [Neg (V "r")]]
exa5 = [[V "p", V "q", V "r"], 
        [V "p", Neg (V "q"), Neg (V "r")],
        [V "p", Neg (V "w")],
        [Neg (V "q"), Neg (V "r"), Neg (V "w")],
        [Neg (V "p"), Neg (V "q"), V "r"],
        [V "u", Neg (V "x")],
        [V "u", V "x"],
        [V "q", Neg (V "u")],
        [Neg (V "r"), Neg (V "u")]]
exa6 = [[V "p"], [Neg (V "p")]]
