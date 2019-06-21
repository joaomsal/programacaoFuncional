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

-- inicializações para testes 
cardapio :: Menu
cardapio = [(150, "Pastel", 1000), (15, "Agua", 400), (2, "Cerveja", 800), (40, "Picanha", 8850), (52, "Pudim", 1275)]
pedidosRest :: PedidosMesas
pedidosRest  =  [[(150,2),(2,2),(15,3)],  [],  [],  [(40,1),(2,2),(52,2)], []]

-- Implementação das Funções 

-- Questão 3.1 a) Adicionar item ao menu
adicionaItemMenu :: Menu -> ItemRest-> Menu
adicionaItemMenu menu (c,n,p) 
    | [ False | (x,y,z) <- menu, x == c ] == [False] = error "\nERROR: item já está no menu!"
    | otherwise = (c,n,p):menu
-- b) Remover item do menu
removeItemMenu :: Menu -> Codigo-> Menu
removeItemMenu menu c = if [(x,y,z) | (x,y,z)<- menu , x/=c]  /= menu then [(x,y,z) | (x,y,z)<- menu , x/=c]
    else error "ERROR: item não consta no menu!"
-- c) Pega item do menu
coletaItemMenu :: Menu -> Codigo-> ItemRest
coletaItemMenu menu c = head[(x,y,z) | (x,y,z)<- menu , x==c]

--Questão 3.2 a) Adiciona um pedido de uma mesana lista de pedidos do restaurante
adicionaPedido:: Mesa-> ItemCliente -> PedidosMesas  -> PedidosMesas
adicionaPedido mesa (c,q) [] =  [[(c,q)]]
adicionaPedido mesa (c,q) pedidoMesa =  inicio ++ ((adiciona getMesa):fim)
    where getMesa = last(take mesa pedidoMesa)
          inicio = init(take mesa pedidoMesa)
          fim = drop mesa pedidoMesa
          adiciona (pedido:getMesa)
            | (fst pedido /= c) = pedido:(adiciona getMesa)
            | (fst pedido == c) = (fst pedido,(snd pedido)+q):getMesa 
            | otherwise = getMesa

-- b) Cancela pedido 
cancelaPedido :: Mesa -> ItemCliente-> PedidosMesas -> PedidosMesas
cancelaPedido mesa (c,q) pedidoMesa = inicio ++ ((remove getMesa):fim)
    where getMesa = last(take mesa pedidoMesa)
          inicio = init(take mesa pedidoMesa) 
          fim = drop mesa pedidoMesa
          remove [] = error "\nERROR: mesa vazia!"
          remove (pedido:getMesa)
            | (fst pedido /= c) = pedido:( remove getMesa)
            | (fst pedido == c) && (snd pedido > q) = (fst pedido,(snd pedido)-q):getMesa 
            | (fst pedido == c) && (snd pedido < q) = getMesa
            | otherwise = getMesa

-- c) pedidos da mesa
pedidoCompletoMesa :: Mesa -> PedidosMesas -> Menu ->[(Quant, Nome, Preco)]
pedidoCompletoMesa mesa pedidos menu = getPedido getMesa
    where getMesa = last(take mesa pedidos)
          getPedido [] = []
          getPedido (pedido:pedidos) = [(snd pedido, nome pedido, preco pedido)]++ (getPedido pedidos)
          nome pedido = head [ b |  (a,b,c)<-[coletaItemMenu menu (fst pedido)], fst pedido == a]
          preco pedido =  head [ c * (snd pedido) |  (a,b,c)<-[coletaItemMenu menu (fst pedido)], fst pedido == a]

-- d) Total da conta de uma mesa
totalMesa :: [(Quant, Nome, Preco)] -> Preco
totalMesa [] = 0
totalMesa (pedido:pedidos) = sum  ([ p | (q,n,p)<-[pedido]]++[totalMesa pedidos])

-- Questão 3.3 a) Formatar preço
formataPreco :: Preco -> String
formataPreco preco =  toString preco
    where toString preco = imprime preco
          imprime preco = ponto (7-(length (saida preco)))++ (saida preco)
          saida preco = show (toDouble preco)++"0"
          toDouble preco = (fromIntegral preco/100)
          ponto n = if n > 0 then "."++ ponto (n-1) else ""
-- b) Formatar pedido
formataLinha :: (Quant,Nome,Preco) -> String
formataLinha (q,n,p) = (saidaQt q) ++ n ++ (pontos n) ++ (saidaP p) 
    where saidaQt q = (esp (length (show q))) ++ show q++ " "
          saidaP p = formataPreco p ++ "\n"
          pontos n = ponto (30 - (length n)) 
          ponto n = if n > 0 then "."++ ponto (n-1) else ""
          esp q = if q == 1 then " " else ""

-- c) Formata lista de pedidos
formataLinhas :: [(Quant,Nome,Preco)] -> String
formataLinhas [] = ""
formataLinhas (pedido:pedidos) = (formataLinha pedido) ++ (formataLinhas pedidos)
-- d) Formata total conta
formataTotal:: [(Quant,Nome,Preco)] -> String
formataTotal mesa =  saida mesa
    where saida mesa = "\n   Total"++ (ponto 25) ++ (formataPreco (total mesa)) ++ "\n"
          total mesa = totalMesa mesa
          ponto n = if n > 0 then "."++ ponto (n-1) else ""

-- e) gera conta formatada
geraConta:: Mesa -> PedidosMesas -> Menu -> IO ()
geraConta mesa ped menu = putStr ("\n"++(formataPedidos mesa ped menu) ++ (formataT mesa ped menu))
    where pedidoCompleto mesa ped menu = pedidoCompletoMesa mesa ped menu 
          formataT mesa ped menu = formataTotal (pedidoCompleto mesa ped menu)
          formataPedidos mesa ped menu = formataLinhas (pedidoCompleto mesa ped menu)

-- f) libera mesa
liberaMesa:: Mesa -> PedidosMesas -> PedidosMesas
liberaMesa mesa pedidoMesa = inicio ++ [] ++ fim 
    where inicio = init(take mesa pedidoMesa)
          fim = drop mesa pedidoMesa
