{-
- Lógica computacional 2020-2
- Practica 1
- Alumno: Mauricio Riva Palacio
- Número de cuenta: 316666343
- Correo: mauricioriva@ciencias.unam.mx
- Alumno: Dicter :v
- Número de cuenta:  
-Correo: 
-}

module Practica1 where

-- Tipos definidos

--Definir el tipo Complejo
type Complejo = (Float, Float)

--Forma para definir el tipo de los números naturales
--de manera recursiva iniciando desde el cero.
data Nat = Cero | Suc Nat deriving(Show,Eq)


--Definición recursiva de listas, la lista más
--pequeña es la lista vacía representada por la palabra Nula
data Lista a = Nula | Cons a (Lista a) deriving(Show,Eq)


--Definición recursiva de árboles, el árbol más pequeño es 
--el árbol vacío.
data Arbol = Vacio | Nodo Arbol Int Arbol deriving(Show,Eq)


-- Funciones principales

--Funcion puntoMedio que dados dos puntos en el plano encuentra el punto medio entre los dos.
--Ejemplo
--Prelude>puntoMedio (-1,2) (7,6)
--(3.0,4.0)
puntoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float)
puntoMedio (x1,y1) (x2,y2) = ((x1+x2)/2, (y1+y2)/2)

--Función que dada una ecuación de segundo grado encuentra las raices de esta en una
--pareja ordenada
raices :: Float -> Float -> Float -> (Complejo,Complejo)
raices 0 _ _ = ((0,0),(0,0))

--Definir la función segmento tal que (segmento m n xs) es la lista de los
--elementos de xs comprendidos entre las posiciones m y n. Por ejemplo,
--segmento 3 4 [3,4,1,2,7,9,0] == [1,2]
--segmento 3 5 [3,4,1,2,7,9,0] == [1,2,7]
--segmento 5 3 [3,4,1,2,7,9,0] == []
segmento :: Int -> Int -> [a] -> [a]
segmento _ _ [] = []
segmento a b (x:xs)
 | a > b = []
 | b == 1 = [x]
 | a == 1 = x:(segmento 1 (b - 1) xs)
 | otherwise = segmento (a - 1) (b - 1) xs

--Definir la función extremos tal que (extremos n xs) es la lista formada
--por los n primeros elementos de xs y los n finales elementos de xs. Por ejemplo,
--extremos 3 [2,6,7,1,2,4,5,8,9,2,3] == [2,6,7,9,2,3]
extremos :: Int -> [a] -> [a]
extremos 0 xs = []
extremos a xs = (segmento 1 a xs) ++ (segmento (longitud xs - a + 1) (longitud xs) xs)

--Funcion que elimina un intervalo de una lista; dados dos números y una lista,
--elimina los elementos que se encuentren en el intervalo de esos dos numeros.
--Por ejemplo,
--dIntervalos 2 4 [1,2,3,4,5,6,7] == [1,5,6,7]
dIntervalos :: Int -> Int -> [a] -> [a]
dIntervalos _ _ [] = []
dIntervalos a b (x:xs)
 | a > b = []
 | b == 1 = xs
 | a == 1 = dIntervalos 1 (b - 1) xs
 | otherwise = x:dIntervalos (a - 1) (b - 1) xs

--Un número natural n se denomina abundante si es menor que la suma de sus divisores
--propios, sin el mismo. Por ejemplo, 12 y 30 son abundantes pero 5 y 28 no lo son.
--Definir la función numerosAbundantes tal que (numerosAbundantes n)
--es la lista de números abundantes menores o iguales que n. Por ejemplo,
--numerosAbundantes 50 == [12,18,20,24,30,36,40,42,48]
numerosAbundantes :: Int -> [Int]
numerosAbundantes 0 = []
numerosAbundantes n
 | n < suma (divProp n) = numerosAbundantes (n - 1) ++ [n]
 | otherwise = numerosAbundantes (n - 1)

--Definir la función que recibe una lista y regrese una lista tal que 
--la lista resultante contiene los mismos elementos que la original pero
--sin duplicados.
--Ejemplo:
--eliminaDuplicados [1,3,1,2,3,2,1] == [1,3,2]
eliminaDuplicados :: Eq a => [a] -> [a]
eliminaDuplicados [] = []
eliminaDuplicados (x:xs) = x:eliminaDuplicados (filter (/= x) xs)

--Se define el primitivo de un número como sigue:
--Dado un número natural n, multiplicamos todos sus dígitos, 
--repetimos este procedimiento hasta que quede un solo dígito al 
--cual llamamos primitivo de n. Por ejemplo, para 327 
--327 : 3 X 2 X 7 = 42 y 4 X 2 = 8. 
--Por lo tanto, el primitivo de 327 es 8.
--Definir la función dado un número nos regrese el primitivo. 
--Ejemplo:
--primitivo 327 == 8
primitivo :: Integer -> Integer
primitivo 0 = 0
primitivo n
 | n < 10 = n
 | otherwise = primitivo (primitivo (div n 10) * mod n 10)

--Función que dadas dos listas y un Natural j, regresa una lista tal que, se encuentran 
--concatenados el i-ésimo elemento de la primer Lista con el i-ésimo elemento de la 
--segunda Lista; a partir del elemento j de cada una de las listas.
--Ejemplo:
--sipLis (Suc (Suc Cero)) (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nula))))) (Cons 7 (Cons 8 (Cons 9 Nula))) ==
--(Cons 2 (Cons 8 (Cons 3 (Cons 9 Nula))))
sipLis :: Nat -> Lista a -> Lista a -> Lista a
sipLis _ Nula ys = Nula
sipLis _ xs Nula = Nula
sipLis (Suc Cero) (Cons x xs) (Cons y ys) = Cons x (Cons y (sipLis (Suc Cero) xs ys))
sipLis (Suc n) (Cons x xs) (Cons y ys) = sipLis n xs ys

--Función la cual convierte un árbol en una lista haciendo un recorrido preorder.
aplanaArbolPre :: Arbol -> [Int]
aplanaArbolPre Vacio = []
aplanaArbolPre (Nodo a n b) = [n] ++ aplanaArbolPre a ++ aplanaArbolPre b

-- Funciones auxiliares en caso de tenerlas van aquí

--Función que devuelve el número de elementos en una lista.
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--Funcion que suma los elementos de una lista.
suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

--Función que dado un numero natural n, te devuelve una lista con todos los divisores propios de n.
divProp :: Int -> [Int]
divProp 1 = [1]
divProp n = [x | x <- [1..n-1], mod n x == 0]

--Función que dado un elemento a y una lista l te dice si el elemento a es elemento de l.
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece a (x:xs)
 | a == x = True
 | otherwise = pertenece a xs

--Función que dadas 2 listas l1,l2 de tamaño m,n devuelve una lista con los elementos
--de l1 y l2 de tamaño m+n, sin perder el orden de los elementos de las listas.
concatena :: Lista a -> Lista a -> Lista a
concatena Nula xs = xs
concatena (Cons x xs) ys = Cons x (concatena xs ys)
