module Abb(Arv, arvVazia, ehVazia, ehNoNulo, arvEsq, arvDir, infoNo, insereNo, removeNo) where
-- TIPOS DO CARDÁPIO
type Codigo = Int
type Nome = String
type Preco = Int
type ItemRest = (Codigo, Nome, Preco) 
type Menu = Arv ItemRest
type Mesa = Int
type Quant = Int
type ItemCliente = (Codigo, Quant)
type PedidoCliente = [ItemCliente]
type PedidosMesas = [PedidoCliente]
type Categoria = String
type DiaSemana = Int
type Categoria = (Int, Int)
-- DEFINIÇÃO DA ÁRVORE
data Arv a =  NoNulo | No a (Arv a) (Arv a) deriving(Eq, Ord, Show, Read) 

-- FUNÇÕES DA ÁRVORE 
arvVazia :: Arv ItemRest
arvVazia = NoNulo

ehVazia :: Arv ItemRest -> Bool
ehVazia (NoNulo) = True
ehVazia _  = False 

ehNoNulo :: Arv ItemRest -> Bool
ehNoNulo NoNulo = True
ehNoNulo _  = False

arvEsq :: Arv ItemRest -> Arv ItemRest
arvEsq (NoNulo) = error "nao tem subarvore esquerda"
arvEsq (No _ tesq _) = tesq

arvDir :: Arv ItemRest -> Arv ItemRest
arvDir (NoNulo) = error "nao tem subarvore direita"
arvDir (No _ _ tdir) = tdir

infoNo :: Arv ItemRest -> ItemRest
infoNo NoNulo = error "arvore vazia"
infoNo (No x _ _) = x

--consultaNo::
insereNo :: (Codigo, Nome, Preco) -> Arv ItemRest -> Arv ItemRest
insereNo (c, n, p) NoNulo = (No (c, n, p) NoNulo NoNulo)
insereNo (c, n, p) (No y tesq tdir)
                    | c == getCod y = (No (c, n, p) tesq tdir)
                    | c > getCod y = No y tesq (insereNo (c, n, p) tdir)
                    | otherwise = No y (insereNo (c, n, p) tesq) tdir
                where getCod (c, n, p) = c

removeNo ::  (Codigo, Nome, Preco) -> Arv ItemRest -> Arv ItemRest
removeNo _ NoNulo = error  "nao há elementos a remover"
removeNo (c, n, p) (No y tesq tdir)
                    | c < getCod y = No y (removeNo (c, n, p) tesq) tdir
                    | c > getCod y = No y tesq (removeNo (c, n, p) tdir)
                    | ehNoNulo tdir = tesq
                    | ehNoNulo tesq = tdir
                    | otherwise = una tesq tdir
                where getCod (c, n, p) = c

una :: Arv ItemRest -> Arv ItemRest -> Arv ItemRest
una tesq tdir = No mini tesq novaArv 
    where (Just mini) = minArv tdir
          novaArv = removeNo mini tdir


minArv :: Arv ItemRest -> Maybe ItemRest
minArv t
        | ehNoNulo t = Nothing
        | ehNoNulo (arvEsq t) = Just (infoNo t)
        | otherwise = minArv (arvEsq t)



-- DEFINIÇÃO DAS VÁRIÁVEIS PARA USO
cardapio :: Menu
--cardapio =  (1,"pp",0) (2,"bb",0)  (2,"arw",1)
cardapio = No (15, "Agua", 400) (No (2, "Cerveja", 800) NoNulo NoNulo) (No (150, "Pastel", 1000) (No (40, "Picanha", 8850) NoNulo NoNulo) (No (52, "Pudim", 1275) NoNulo NoNulo))
ca :: Menu
ca = No (10,"pp",0) (No (3,"pp",0) NoNulo NoNulo) NoNulo
-- RESPOSTAS DA LISTA
-- QUESTÃO 2
-- A) COLETA ITEM PELO COD
coletaItemMenu :: Menu -> Codigo -> ItemRest
coletaItemMenu (NoNulo) _ =  error "ITEM NÃO CONSTA NO MENU"
coletaItemMenu menu c 
                    | getCod cab == c = cab
                    | otherwise = coletaItemMenu (removeNo cab menu) c
                where getCod (c,n,p) = c 
                      cab = infoNo menu

-- B) COLETA ITEM PELO NOME
coletaItemMenu2 :: Menu -> Nome-> ItemRest
coletaItemMenu2 (NoNulo) _ =  error "ITEM NÃO CONSTA NO MENU"
coletaItemMenu2 menu n 
                    | getNome cab == n = cab
                    | otherwise = coletaItemMenu2 (removeNo cab menu) n
                where getNome (c,n,p) = n 
                      cab = infoNo menu

-- C) ATUALIZA PREÇOS
atualizaPrecosMenu :: Menu -> Int -> Menu
atualizaPrecosMenu (NoNulo) _ = NoNulo
atualizaPrecosMenu (No cb eq dr) a =  (No (up cb) (atualizaPrecosMenu eq a) (atualizaPrecosMenu dr a)) 
    where up (c,n,p) = (c, n, p+(div (a*p) 100))

-- D) ATUALIZA PREÇOS POR CATEGORIA
