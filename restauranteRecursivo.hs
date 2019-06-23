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

