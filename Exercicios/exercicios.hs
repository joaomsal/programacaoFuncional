import Data.Char
-- USANDO MAP
-- Converte lista de caracteres em lista de codigos
toCod :: [Char] -> [Int]
toCod xs = map ord xs

-- Quadrados de lista de inteiros
toQuad :: [Int] -> [Int]
toQuad xs = map quad xs
    where quad x = x*x

-- Segundos elementos de uma lista de tuplas
getSnd :: [(Int,Int)] -> [Int]
getSnd xs = map snd xs

-- Fazer meu próprio Length
myLength :: [a] -> Int
myLength xs = sum (map cnt xs)
    where cnt x = 1

-- USANDO ZIPWITH
-- dobro dos elementos de uma lista
toDobro :: [Int] -> [Int]
toDobro xs = zipWith (+) xs xs

-- quadrados dos elementos de uma lista
toQuad2 :: [Int] -> [Int]
toQuad2 xs = zipWith (*) xs xs

-- verfica a presença de digitos em uma string
hasDigit :: [Char] -> Bool
hasDigit xs = or (map isDigit xs)

-- FILTER
-- pega os apenas os pares de uma lista 
pares :: [Int] -> [Int]
pares xs = filter par xs
    where par x = (mod x 2) == 0 

-- pega os apenas os impares de uma lista 
impares :: [Int] -> [Int]
impares xs = filter par xs
    where par x = (mod x 2) /= 0 

-- pega apenas os digitos de uma string
getNumbers :: [Char] -> [Char]
getNumbers xs = filter isDigit xs

-- pega apenas os caracteres de uma string
getChars :: [Char] -> [Char]
getChars xs = filter notIsDigit xs
    where notIsDigit x = not (isDigit x)

-- pega apenas as letras minusculas
getMinus :: [Char] -> [Char]
getMinus xs = filter isMin xs
    where isMin x = (x >= 'a') && (x <= 'z')

-- positivos de uma lista
getPos :: [Int] -> [Int]
getPos xs = filter pos xs
    where pos x = x>=0

-- pega pares ordenados de uma lista
getOrdPares :: [(Int, Int)] -> [(Int, Int)]
getOrdPares xs = filter isOrd xs
    where isOrd (x,y) = x<=y

-- pega pares tipo (a, a²)
getParQ :: [(Int, Int)] -> [(Int, Int)]
getParQ xs = filter pq xs
    where pq (x,y) = y == (x*x)

-- pega o indice da ultima ocorrecia de x em um String
getLastX :: Char -> [Char] -> Int
getLastX c xs = if fnc == [] then -1 else snd (head fnc) 
    where aux = zip xs [1..]
          fnc = filter ult (reverse aux)
          ult (x, i) = c == x
          
-- GENERALIZAÇÃO
-- aplicação de função n vezes
iter :: Int-> (a -> a) -> (a -> a)
iter n f = it n f
    where it 1 g = g
          it x g = g.(it (x-1) g)

-- função auxiliar dobro
dobro :: Int -> Int
dobro x = x+x
-- pontenciação
pot :: Int -> Int
pot 0 = 1 
pot x = (iter (x-1) dobro) 2

-- composição de funções
dobroImpar :: [Int] -> [Int]
dobroImpar xs = (toDobro.impares) xs 

-- quantidade de caracteres em uma string
qtChar :: [Char] -> Int
qtChar xs = (myLength.getChars) xs

-- quantidade de digitos em uma string
qtNums :: [Char] -> Int
qtNums xs = (myLength.getNumbers) xs

-- primeira palavra de uma string
fstWord :: [Char] -> [Char]
fstWord xs = takeWhile vazio xs
    where vazio x = x /= ' '
