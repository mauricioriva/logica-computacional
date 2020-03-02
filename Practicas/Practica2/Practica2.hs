{-
- Lógica computacional 2020-2
- Practica 1
- Alumno: Mauricio Riva Palacio
- Número de cuenta: 316666343
- Correo: mauricioriva@ciencias.unam.mx
- Alumno: Garcia Rosas Dicter Tadeo
- Número de cuenta:  316085412
-Correo: dicteraulad@ciencias.unam.mx
-}

module Practica2 where


-- ---------------------------------------------------------------------
-- Definimos los siguientes tipos de datos:
-- Prop para representar las fórmulas proposicionales usando los
-- constructores T, F, Var, Neg, Conj, Disy, Impl y Equi para las fórmulas
-- atómicas, negaciones, conjunciones, implicaciones y equivalencias,
-- respectivamente.
-- ---------------------------------------------------------------------

data Prop = T | F | Var String | Neg Prop | Conj Prop Prop | Disy Prop Prop | Impl Prop Prop | Equi Prop Prop deriving Eq

type Estado = [String]

instance Show Prop where
         show T = "Verdadero"
         show F = "Falso"
         show (Var p) = p
         show (Neg p) = "¬" ++ show p
         show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
         show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
         show (Impl p q) = "(" ++ show p ++ " ⟶  " ++ show q ++ ")"
         show (Equi p q) = "(" ++ show p ++ " ⟷  " ++ show q ++ ")"


-- ---------------------------------------------------------------------
-- Definimos las siguientes fórmulas proposicionales
-- como variables atómicas: p, q, r, s, t, u.
-- ---------------------------------------------------------------------
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"


-- ---------------------------------------------------------------------
-- Símbolos proposicionales de una fórmula --
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 1: Definir la función
-- variables :: Prop -> [String]
-- tal que (variables f) es el conjunto formado por todos los
-- símbolos proposicionales que aparecen en f. Por ejemplo,
-- >variables (Impl (Conj (Var "p") (Var "q")) (Var "p"))
-- ["p","q"]
-- >variables Conj (Var "q") (Disy (Var "r") (Var "p"))
-- ["q","r","p"]
-- ---------------------------------------------------------------------

variables :: Prop -> Estado
variables (Var p) = [p]
variables (Neg p) = variables p
variables (Disy p q) = unn(variables p) (variables q)
variables (Conj p q) = unn(variables p) (variables q)
variables (Impl p q) = unn(variables p) (variables q)
variables (Equi p q) = unn(variables p) (variables q)


-- ---------------------------------------------------------------------
-- funcion auxiliar que elimina los repetidos en una lista.
-- ---------------------------------------------------------------------


eliminaRepetidos :: (Eq a) => [a] ->[a]
eliminaRepetidos [] = []
eliminaRepetidos (x:xs) = x:(eliminaRepetidos (filter (/= x) xs))


-- ---------------------------------------------------------------------
-- funcion auxiliar que nos ayuda a obtener una lista con los elementos
-- sin repetidos.
-- ---------------------------------------------------------------------


unn :: Eq a => [a] -> [a] -> [a]
unn xs ys = eliminaRepetidos(eliminaRepetidos(xs) ++ eliminaRepetidos(ys))


-- ---------------------------------------------------------------------
-- Ejercicio 2: Definir la función
-- conjPotencia :: [a] -> [[a]]
-- tal que (conjPotencia x) es la lista de todos los subconjuntos de x.
-- Por ejmplo,
-- >conjPotencia [1,2]
-- [[]; [2]; [1]; [1; 2]]
-- >conjPotencia []
-- [[]]
-- >conjPotencia "abc"
-- ["abc","ab","ac","a","bc","b","c",""]
-- ---------------------------------------------------------------------

conjPotencia :: (Eq a) => [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = (conjPotencia xs) ++ [(x:ys) | ys <- (conjPotencia xs)]

-- ---------------------------------------------------------------------
-- Interpretaciones --
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 3: Definir la función
-- interpretacion :: Prop -> Estado -> Bool
-- tal que (interpretacion f e) es la interpretación de f en e. Por ejemplo,
-- interpretacion Conj (Var "q") (Disy (Var "r") (Var "p")) ["p"]
-- False
-- >interpretacion Conj (Var "q") (Disy (Var "r") (Var "p")) ["p","q"]
-- True
-- ---------------------------------------------------------------------

interpretacion :: Prop -> Estado -> Bool
interpretacion (Var x) ys = (busca x ys) == True
interpretacion (T) ys = True
interpretacion (F) ys = False
interpretacion (Neg p) ys = not (interpretacion p ys)
interpretacion (Disy p q) ys = (interpretacion p ys) || (interpretacion q ys)
interpretacion (Conj p q) ys = (interpretacion p ys) && (interpretacion q ys)
interpretacion (Impl p q) ys = (interpretacion (Neg p) ys) || (interpretacion q ys)
interpretacion (Equi p q) ys = interpretacion (eliminacion (Equi p q)) ys


-- ---------------------------------------------------------------------
-- Función auxiliar que recibe una fórmula y devuelve una equivalente
-- eliminando las implicaciones y las equivalencias
-- ---------------------------------------------------------------------

eliminacion :: Prop -> Prop
eliminacion (Var x) = Var x
eliminacion (T) = T
eliminacion (F) = F
eliminacion (Neg(Neg p)) = eliminacion p
eliminacion (Neg p) = Neg (eliminacion p)
eliminacion (Disy p q) = Disy (eliminacion p) (eliminacion q)
eliminacion (Conj p q) = Conj (eliminacion p) (eliminacion q)
eliminacion (Impl p q) = Disy (Neg (eliminacion p)) (eliminacion q)
eliminacion (Equi p q) = Conj (eliminacion (Impl p q)) (eliminacion (Impl q p))


-- ---------------------------------------------------------------------
-- Funcion auxiliar que busca a la variable dentro de la lista.
-- ---------------------------------------------------------------------
busca :: String -> Estado -> Bool
busca n [] = False
busca n (x:xs)
 |(n == x) = True
 |otherwise = busca n xs

-- ---------------------------------------------------------------------
-- Ejercicio 4: Definir una función que dada una fórmula proposicional,
-- la función devuelve todos los estados con los que podemos evaluar
-- la fórmula. Por ejemplo,
-- >estadosPosibles Disy (Var "q") (Conj (Var "r") (Var "q"))
-- [[],["q"]; ["r"]; ["q","r"]]
-- ---------------------------------------------------------------------

estadosPosibles :: Prop -> [Estado]
estadosPosibles p = conjPotencia(variables p)

-- ---------------------------------------------------------------------
-- Ejercicio 5: Definir una función que dada una fórmula proposicional,
-- nos diga si es una tautología. Por ejemplo,
-- >tautologia Disy (Var "p") (Neg (Var "p"))
-- True
-- >tautologia Disy (Var "q") (Var "r")
-- False
-- ---------------------------------------------------------------------

tautologia :: Prop -> Bool
tautologia p = if ((filter (\x -> x == True) (map (interpretacion p) (estadosPosibles p))) == (map (interpretacion p) (estadosPosibles p))) then True
  else False


-- ---------------------------------------------------------------------
-- Ejercicio 6: Definir una función que dada una fórmula proposicional,
-- nos diga si es una contradicción. Por ejemplo,
-- >contradiccion Disy (Var "p") (Neg (Var "p"))
-- False
-- >contradiccion (Disy (Var "q") (Var "r"))
-- True
-- ---------------------------------------------------------------------

contradiccion :: Prop -> Bool
contradiccion p = if((filter (\x -> x == False) (map (interpretacion p) (estadosPosibles p))) == (map (interpretacion p) (estadosPosibles p))) then True
else False


-- ---------------------------------------------------------------------
-- Modelos --
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Ejercicio 7: Definir una función que dada una interpretación y una
-- fórmula proposicional, verifique si esta interpretación es un modelo.
-- Por ejemplo,
-- >esModelo ["r"] (Conj (Disy p q) (Disy (Neg q) r))
-- False
-- >esModelo ["p","r"] (Conj (Disy p q) (Disy (Neg q) r))
-- True
-- ---------------------------------------------------------------------

esModelo :: Estado -> Prop -> Bool
esModelo ys (Var x) = (busca x ys) == True
esModelo ys (T) = True
esModelo ys (F) = False
esModelo ys (Neg p) = not (esModelo ys p)
esModelo ys (Disy p q) = (esModelo ys p) || (esModelo ys q)
esModelo ys (Conj p q) = (esModelo ys p) && (esModelo ys q)
esModelo ys (Impl p q) = (esModelo ys (Neg p)) || (esModelo ys q)
esModelo ys (Equi p q) = esModelo (ys) (eliminacion (Equi p q))


-- ---------------------------------------------------------------------
-- Ejercicio 8: Definir una función que dada una fórmula proposicional
-- devuelve la lista de todos sus modelos; tal que (modelos f) es la
-- lista de todas las interpretaciones de f que son modelo. Por ejemplo,
-- >modelos (Conj (Disy p q) (Disy (Neg q) r))
-- [["p","q","r"],["p","r"],["p"],["q","r"]]
-- ---------------------------------------------------------------------

modelos :: Prop -> [Estado]
modelos p = [(x) | x <- estadosPosibles(p), interpretacion p x == True]

-- ---------------------------------------------------------------------
-- Ejercicio 9: Definir una función que dada una fórmula proposicional f
-- verifica si f es válida. Por ejemplo,
-- esValida (Impl p p)
-- True
-- esValida (Impl p q)
-- False
-- esValida (Disy (Impl p q) (Impl q p))
-- True
-- ---------------------------------------------------------------------

esValida :: Prop -> Bool
esValida p = tautologia p

-- ---------------------------------------------------------------------
-- Ejercicio 10: Definir una función que dada una fórmula proposicional f
-- verifica si f es insatisfacible. Por ejemplo,
-- esInsatisfacible (Conj p (Neg p))
-- True
-- esInsatisfacible (Conj (Impl p q) (Impl q r))
-- False
-- ---------------------------------------------------------------------

esInsatisfacible :: Prop -> Bool
esInsatisfacible p = modelos(p) == []

-- ---------------------------------------------------------------------
-- Ejercicio 11: Definir una función que dada una fórmula proposicional f
-- verifica si f es satisfacible. Por ejemplo,
-- esSatisfacible (Conj p (Neg p))
-- False
-- esSatisfacible (Conj (Impl p q) (Impl q r))
-- True
-- ---------------------------------------------------------------------

esSatisfacible :: Prop -> Bool
esSatisfacible p = modelos(p) /= []
