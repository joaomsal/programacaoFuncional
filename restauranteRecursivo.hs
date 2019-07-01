-- Funções feitas no arquivo restaurante.hs, utilizando recursão

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

-- Questão 4.1 a) Adicionar item ao menu
adicionaItemMenu :: Menu -> ItemRest -> Menu
adicionaItemMenu [] (cod,nome,preco) = (cod,nome,preco):[]
adicionaItemMenu (item:menu) (cod,nome,preco) 
                        | verificaItem cod [item] = error "\nERROR: Código já cadastrado no menu!"
                        | otherwise = item: adicionaItemMenu menu (cod,nome,preco)
                    where verificaItem cod item = head[ x == cod | (x,y,z) <- item ] 

-- b) remover item do menu 
removeItemMenu :: Menu -> Codigo -> Menu
removeItemMenu [] _ = []
removeItemMenu  (item:menu) cod 
                        | (verificaItem [item] cod) /= [] = [item] ++ removeItemMenu menu cod
                        | otherwise =  [] ++ removeItemMenu menu cod
                    where verificaItem item c = [ (x,y,z) |(x,y,z) <- item , x/=c]

-- c) Pega item do menu 
coletaItemMenu :: Menu -> Codigo -> ItemRest
coletaItemMenu (item:menu) cod 
                        | verifica item cod = item
                        | otherwise = coletaItemMenu menu cod
                    where verifica (c,n,p) cod = c==cod

-- Questçao 4.2 a) adicionar pedido a mesa 
adicionaPedido:: Mesa->  ItemCliente ->  PedidosMesas -> PedidosMesas
adicionaPedido mesa (c,q) pedidos = inicio ++ ((mesaRecursiva getMesa):fim)
            where mesaRecursiva [] =  [(c,q)]
                  mesaRecursiva (pedido:mesa)
                        | (fst pedido) == c = (c, (snd pedido)+q):mesa
                        | otherwise = pedido:(mesaRecursiva mesa)
                  getMesa = last(take mesa pedidos)
                  inicio = init(take mesa pedidos)
                  fim = drop mesa pedidos

-- b) cancela pedido
cancelaPedido:: Mesa -> ItemCliente->   PedidosMesas -> PedidosMesas
cancelaPedido mesa (c,q) pedidos = inicio ++ ((mesaRecursiva getMesa):fim)
            where mesaRecursiva [] =  error "\nERROR: Item não encontrado!"
                  mesaRecursiva (pedido:mesa)
                        | (fst pedido) == c && (snd pedido) > q = (c, (snd pedido)-q):mesa
                        | (fst pedido) == c && (snd pedido) <= q = mesa
                        | otherwise = pedido:(mesaRecursiva mesa)
                  getMesa = last(take mesa pedidos)
                  inicio = init(take mesa pedidos)
                  fim = drop mesa pedidos

-- c) pedidos detalhado por mesa
pedidoCompletoMesa :: Mesa -> PedidosMesas -> Menu -> [(Quant,Nome, Preco)]
pedidoCompletoMesa mesa [] menu = []
pedidoCompletoMesa mesa pedidos menu = pedidoRecursivo getMesa
            where getMesa = last(take mesa pedidos)
                  pro pedido = head [ b |  (a,b,c)<-[coletaItemMenu menu (fst pedido)], fst pedido == a]
                  pre pedido =  head [ c * (snd pedido) |  (a,b,c)<-[coletaItemMenu menu (fst pedido)], fst pedido == a]
                  pedidoRecursivo [] = []
                  pedidoRecursivo (pedido:getMesa)  = [((snd pedido), (pro pedido), (pre pedido))] ++ (pedidoRecursivo getMesa)
  