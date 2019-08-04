-- Lista 02 - PROGRAMAÇÃO FUNCIONAL
-- JOÃO MANOEL SANTOS ALMEIDA - 201600016896

-- Definição dos tipos 
type Codigo = Int
type Nome = String
type Preco = Int
type ItemRest = (Codigo, Nome, Preco)
type Menu = [ItemRest]
type Mesa = Int
type Quant = Int
type ItemCliente = (Codigo, Quant)
type PedidoCliente = [ItemCliente]
type PedidosMesas = [PedidoCliente]
type Categoria = String
-- inicializações para testes 
cardapio :: Menu
cardapio = [(150, "Pastel", 1000), (15, "Agua", 400), (2, "Cerveja", 800), (40, "Picanha", 8850), (52, "Pudim", 1275)]
pedidosRest :: PedidosMesas
pedidosRest  =  [[(150,2),(2,2),(15,3)],  [],  [],  [(40,1),(2,2),(52,2)], []]


-- QUESTÃO 1
-- ITEM a) COLETAR ITEM MENU
coletaItemMenu :: Menu -> Codigo -> ItemRest    
coletaItemMenu menu codigo = head (filter isItem menu)
    where isItem (cod, nome, preco) = codigo == cod

-- ITEM b) ATUALIZAR PREÇOS
atualizaPrecosMenu :: Menu -> Int-> Menu
atualizaPrecosMenu menu pct = map atualiza menu
    where atualiza (cod, nome, preco) = (cod, nome, preco+(div (pct*preco) 100))

-- ITEM c) ATUALIZAR PREÇOS POR CATEGORIA
atualizaPrecosCat:: Menu -> Categoria -> Int-> Menu
atualizaPrecosCat menu cat pct = map inCat menu
    where inCat (cod, nome, preco) 
                            | cod >= menor && cod <= maior =  (cod, nome, preco+(div (pct*preco) 100))
                            | otherwise = (cod, nome, preco)
          menor = fst (catMaiorMenorCod cat)
          maior = snd (catMaiorMenorCod cat)
-- FUNÇÃO AUXILIAR
catMaiorMenorCod :: Categoria -> (Codigo,Codigo)
catMaiorMenorCod cat 
                | cat == "Bebidas" = (1,50)
                | cat == "Tira-gostos" = (51,100)
                | cat == "Carnes" = (101,120)
                | cat == "Aves" = (121,140)
                | cat == "Peixes e Maricos" = (141,160)
                | cat == "Massas" = (161,180)
                | cat == "Acompanhamentos extras" = (181,200)
                | cat == "Sobremesas" = (201,220)

-- QUESTÃO 2
-- ITEM a) ADICIONA PEDIDO
adicionaPedido:: Mesa-> ItemCliente -> PedidosMesas -> PedidosMesas
adicionaPedido mesa (cod, qt) pedidos =   map (snd) adiciona 
    where mesas = zip [1..] pedidos
          getMesa = filter isMesa mesas
          isMesa m = fst m == mesa
          adiciona = map addMesa mesas
          addMesa (m,p) 
            | m == mesa = (m, addPed p)
            | otherwise = (m,p)
          addPed y 
            | filter filtro y /= [] =  map add y
            | otherwise = [(cod, qt)] ++ y
          add (m,n) 
            | m == cod = (cod, qt + n)
            | otherwise = (m,n) 
          filtro x = fst x == cod

-- ITEM b) CANCELA PEDIDO
cancelaPedido :: Mesa -> ItemCliente-> PedidosMesas -> PedidosMesas
cancelaPedido mesa (cod, qt) pedidos = map (snd) cancela 
      where mesas = zip [1..] pedidos
            getMesa = filter isMesa mesas
            isMesa m = fst m == mesa
            cancela = map clMesa mesas
            clMesa (m,p) 
              | m == mesa = (m, exPed p)
              | otherwise = (m,p)
            exPed y 
              | filter filtro y /= [] = filter items (map ex y)
              | otherwise = [(cod, qt)] ++ y
            ex (m,n) 
              | m == cod && qt < n = (cod, n - qt)
              | m == cod && qt >= n = (-1, 0)
              | otherwise = (m,n) 
            filtro x = fst x == cod
            items x = fst x /= -1

-- ITEM c) ORDENA DE PEDIDOS
pedidoOrdenado:: Mesa -> PedidosMesas -> PedidosMesas
pedidoOrdenado mesa pedidos = foldr insOrd [] getMesa
  where mesas = zip [1..] pedidos
        getMesa = filter isMesa mesas
        isMesa m = fst m == mesa
        insOrd:: Int-> [ItemCliente] -> [ItemCliente]
        insOrd y [] = [y] 
        insOrd y (z:zs) 
              | y <= fst z = y : z : zs
              | otherwise = z: insOrd y zs