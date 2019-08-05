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
type DiaSemana = Int
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
pedidoOrdenado mesa pedidos = map ordInsercao (map snd getMesa)
  where mesas = zip [1..] pedidos
        getMesa = filter isMesa mesas
        isMesa m = fst m == mesa
-- FUNÇÃO DE ORDENAÇÃO ADAPTADA         
ordInsercao :: [ItemCliente] -> [ItemCliente]
ordInsercao xs = foldr insOrd [] xs
  where insOrd:: ItemCliente -> [ItemCliente] -> [ItemCliente]
        insOrd (cod,qt) [] = [(cod,qt)] 
        insOrd (cod,qt) (z:zs) 
          | cod <= fst z = (cod,qt) : z : zs
          | otherwise = z: insOrd (cod,qt) zs

-- ITEM d) PEDIDO MESA
pedidoCompletoMesa :: Mesa -> PedidosMesas -> Menu -> [(Quant,Nome, Preco)]
pedidoCompletoMesa mesa pedidos menu =  map getPedido pedidosOrd
    where mesas = zip [1..] pedidos
          getMesa = filter isMesa mesas
          isMesa m = fst m == mesa
          pedidosOrd = head (pedidoOrdenado mesa pedidos)
          getPedido (cod, qt) = (qt, getNome cod , (getPreco cod) * qt )
          getNome cd = nome (head (filter (isItem cd) menu))
          getPreco cd = preco (head (filter (isItem cd) menu))
          isItem c (cod, nome , preco) = cod == c
          nome (cod, nome , preco) = nome
          preco (cod, nome , preco) = preco

-- QUESTÃO 3
-- ITEM a) DESCONTOS
desconto:: [(Quant,Nome,Preco)] -> DiaSemana -> Int -> [(Quant,Nome,Preco)]
desconto pedidos dia pct = appDesconto
    where appDesconto
              | dia == 1 = map (desconta (1,50)) pedidos
              | dia == 2 = map (desconta (141,160)) pedidos
              | dia == 3 = map (desconta (51,100)) pedidos
              | dia == 4 = map (desconta (101,140)) pedidos
              | dia == 5 = map (desconta (161,180)) pedidos
              | dia == 6 = map (desconta (1,50)) pedidos
              | dia == 7 = map (desconta (181,220)) pedidos
           -- desconta (qt, nm, pr) = (qt, nm, pr+(div (pct*pr) 100))
          desconta (menor, maior) (qt, nome, preco) 
                  | getCod (head (filter (isCod nome) cardapio)) >= menor && getCod (head (filter (isCod nome) cardapio)) <= maior =  (qt, nome, preco-(div (pct*preco) 100))
                  | otherwise = (qt, nome, preco)
          getCod (c, n, p) = c
          isCod m (c, n, p) = n  == m

-- QUESTÃO 4 
-- ITEM a) TOTAL MESA
totalMesa :: [(Quant, Nome, Preco)] -> Preco
totalMesa pedidos = sum (map total pedidos)
    where total (qt,n,p) = p

-- ITEM b) FORMATAÇÃO
formataLinhas:: [(Quant,Nome,Preco)] -> String
formataLinhas xs = ip (map print imprime)
  where formatacao = map formataLinha xs
        imprime = map print formatacao
        print x = x 
        ip [] = []
        ip (x:xs)  = x ++ ip xs

-- FUNÇÕES AUXILIARES 
formataPreco :: Preco -> String
formataPreco preco =  toString preco
    where toString preco = imprime preco
          imprime preco = ponto (7-(length (saida preco)))++ (saida preco)
          saida preco = show (toDouble preco)++"0"
          toDouble preco = (fromIntegral preco/100)
          ponto n = if n > 0 then "."++ ponto (n-1) else ""
-- Formatar pedido
formataLinha :: (Quant,Nome,Preco) -> String
formataLinha (q,n,p) = (saidaQt q) ++ n ++ (pontos n) ++ (saidaP p) 
    where saidaQt q = (esp (length (show q))) ++ show q++ " "
          saidaP p = formataPreco p ++ "\n"
          pontos n = ponto (30 - (length n)) 
          ponto n = if n > 0 then "."++ ponto (n-1) else ""
          esp q = if q == 1 then " " else ""