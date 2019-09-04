-- TIPOS ALGÉBRICOS
type Pessoa = String
type Livro = String
data Emp = Emprestimo Pessoa Livro deriving (Eq, Show)

emprestimos :: [Emp]
emprestimos = [Emprestimo "Bruce" "Sherlock Holmes", Emprestimo "Diana" "Cinderela", Emprestimo "Bruce" "Artes Marciais",
                Emprestimo "Diana" "Artes Marciais", Emprestimo "Oliver" "Artes Marciais"]
-- CONSULTA LIVROS EMPRESTADOS POR UMA DETERMINADA PESSOA
consultaEmps :: Pessoa -> [Emp] -> [Livro]
consultaEmps _ [] = []
consultaEmps p ((Emprestimo ps l):xs)
                    | p == ps = [l] ++ consultaEmps p xs
                    | otherwise = consultaEmps p xs
-- CONSULTA PESSOAS QUE POSSUEM UM DETERMINADO LIVRO EMPRESTADO
consultaEmps2 :: Livro -> [Emp] -> [Livro]
consultaEmps2 _ [] = []
consultaEmps2 l ((Emprestimo ps lv):xs)
                    | l == lv = [ps] ++ consultaEmps2 l xs
                    | otherwise = consultaEmps2 l xs
-- CONSULTA SE UM DETERMINADO LIVRO ESTÁ EMPRESTADO
estaEmprestado :: Livro -> [Emp] -> Bool
estaEmprestado livro emps
                    | consultaEmps2 livro emps == [] = False
                    | otherwise = True

-- CONSULTA QUANTOS LIVROS UMA PESSOA PEGOU EMPRESTADO
contEmps :: Pessoa -> [Emp] -> Int
contEmps p emps = count (consultaEmps p emps)
    where count [] = 0
          count (x:xs) = 1 + count xs 
-- ADICIONA UM EMPRÉSTIMO NA LISTA DE EMPRESTIMOS 
addEmp :: Emp -> [Emp] -> [Emp]
addEmp emp emps = emp:emps

-- REMOVE UM EMPRÉSTIMO DA LISTA DE EMPRESTIMOS 
rmvEmp :: Emp -> [Emp] -> [Emp]
rmvEmp emp [] = []
rmvEmp em (e:emps)
            | em == e = rmvEmp em emps
            | otherwise = [e] ++ rmvEmp em emps

juntaEmps :: [Emp] -> [(Pessoa, [Livro])]
juntaEmps [] = []
juntaEmps emps =  result (junta emps)
    where result ((p, ls):es) = [(p, ls)] ++ rmv p es
          junta ((Emprestimo ps lv):xs) = [(ps, [lv] ++ (consultaEmps ps xs))] ++ juntaEmps xs
          rmv _ [] = []
          rmv p ((ps,ls):es) 
                        | p == ps = rmv p es
                        | otherwise = [(ps,ls)] ++ rmv p es

-- FIM DO ESCOPO DE EMPRESTIMO ###################################################

-- EXPRESSÃO
data Ops =  Add | Sub | Mult deriving (Show)
data Expr = Lit Integer | Op Ops Expr Expr deriving (Show)
-- EXEMPLO
exp1 :: Expr
exp1 =  (Op Add ((Op Add (Lit 1) (Lit 2))) ((Op Add (Lit 1) (Lit 2))) )
-- CONTABILIZA OS OPERADORES
contaOps :: Expr -> Int
contaOps (Lit x) = 0
contaOps (Op ops expr1 expr2) = 1 + (contaOps expr1) + (contaOps expr2)

