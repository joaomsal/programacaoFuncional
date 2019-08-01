-- 201600016896 - JOÃO MANOEL SANTOS ALMEIDA 
-- Lista Programação Funcional (TURMA 05)
-- Questão 1 :
-- a) Quatro iguais
quatroIguais :: Int -> Int -> Int -> Int -> Bool
quatroIguais x y u v = (x==y && y==u && u == v ) 
-- Três iguais, feito pela prof.
tresIguais:: Int-> Int-> Int-> Bool
tresIguais x y z = (x == y) && (y == z)
-- b) Quatro iguais usando Três iguais
quatroIguaisNew :: Int -> Int -> Int -> Int -> Bool
quatroIguaisNew x y u v = (tresIguais x y u) && u == v
-- c) Item a) usando Let
quatroIguaisLet :: Int -> Int -> Int -> Int -> Bool
quatroIguaisLet x y u v =
    let tresIguais = (x == y) && (y == u)
    in tresIguais && (u==v)

-- Questão 2:
-- a) Três Iguais
tresDiferentes :: Int-> Int-> Int-> Bool
tresDiferentes x y z = (x/=y) && (y/=z) && (x/=z)
-- b) Quantos Iguais
conta3Iguais :: Int-> Int-> Int-> Int
conta3Iguais x y z
    | tresDiferentes x y z = 0
    | tresIguais x y z = 3
    | x == y || x == z || y == z = 2
-- c) Usando item b) para 4 nº
conta4Iguais :: Int-> Int-> Int-> Int-> Int
conta4Iguais x y u v 
    | (conta3Iguais x y u == 3) && x == v = 4
    | (conta3Iguais x y u == 3) || (conta3Iguais x y v == 3) || (conta3Iguais x v u == 3) || (conta3Iguais v y u == 3) = 3
    | (conta3Iguais x y u == 2) || (conta3Iguais x y v == 2) || (conta3Iguais x v u == 2) || (conta3Iguais v y u == 2) = 2
    | otherwise = 0
