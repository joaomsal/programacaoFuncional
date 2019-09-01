import Abb
-- JOÃO MANOEL SANTOS ALMEIDA - 201600016896
-- TURMA 05
-- Definição dos tipos 
type Codigo = Int
type Nome = String
type Preco = Int
type ItemRest = (Codigo, Nome, Preco)
--type Menu = Arv No ItemRest ItemRest ItemRest
type Arb = Arv ItemRest
type Mesa = Int
type Quant = Int
type ItemCliente = (Codigo, Quant)
type PedidoCliente = [ItemCliente]
type PedidosMesas = [PedidoCliente]
type Categoria = String
type DiaSemana = Int

-- inicializações para testes 
--cardapio :: Arb
--cardapio =  (1,"pp",0) (2,"bb",0)  (2,"arw",1)
--cardapio = Arv  No (1,"pp",0) (No (2,"bb",0) NoNulo NoNulo) (No (2,"arw",1) NoNulo NoNulo)
--cardapio = [(150, "Pastel", 1000), (15, "Agua", 400), (2, "Cerveja", 800), (40, "Picanha", 8850), (52, "Pudim", 1275)]
pedidosRest :: PedidosMesas
pedidosRest  =  [[(150,2),(2,2),(15,3)],  [],  [],  [(40,1),(2,2),(52,2)], []]

test:: Int -> Int
test x = x+32