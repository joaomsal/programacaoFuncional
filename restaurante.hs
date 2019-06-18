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
pedidosRest  =  [[(150,1),(2,2)],  [],  [],  [(40,1),(2,2),(52,2)], []]

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
-- [(c,(snd pedido)+q) | pedidoMesa<-pedidosMesas, pedido<-pedidoMesa, (fst pedido)==c]
-- [xss ++ yss| (xss,yss) <-(splitAt m pedidosMesas) , pedido<-(last xss), (u,v)<-pedido, v+q ]
{- adicionaPedido:: Mesa-> ItemCliente -> PedidosMesas  -> PedidosMesas
adicionaPedido m (c,q) [] = [[(c,q)]] 
adicionaPedido m (c,q) (pedido:pedidosMesas) 
    | pedido  /= [] = [pedido]
    | otherwise = [] -}
{- adicionaPedido:: Mesa-> ItemCliente -> PedidosMesas  -> PedidosMesas
adicionaPedido m (c,q) [] = [[(c,q)]] 
adicionaPedido m (c,q) pedidosMesas 
        | True = [ [(c,(snd pedidos)+q)] | pedidos<-head [ [pedido] | pedido <- last (fst (splitAt m pedidosMesas)), resto<- fst (splitAt m pedidosMesas)], c==(fst pedidos)]  -}


adicionaPedido:: Mesa-> ItemCliente -> PedidosMesas  -> PedidosMesas
adicionaPedido mesa item pedidos = [[inicio] ++ [editado] ++ [fim]]
    where inicio = take (m-1) pedidos
          editado = item 
          fim = drop m pedidos