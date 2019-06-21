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
pedidosRest  =  [[(150,2),(2,2)],  [],  [],  [(40,1),(2,2),(52,2)], []]

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
pedidoCompletoMesa mesa pedidos menu = [getPedido]
    where getMesa = last(take mesa pedidos)
          getPedido = (codigo getMesa, nome getMesa, preco getMesa)
          nome (pedido:pedidos) = head [ b |  (a,b,c)<-[coletaItemMenu menu (fst pedido)], fst pedido == a]
          preco (pedido:pedidos) =  head [ c * (snd pedido) |  (a,b,c)<-[coletaItemMenu menu (fst pedido)], fst pedido == a]
          codigo (pedido:pedidos) =  head [ a |  (a,b,c)<-[coletaItemMenu menu (fst pedido)], fst pedido == a]