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

consultaNo:: (Codigo, Nome, Preco) -> Arv ItemRest -> Bool
consultaNo _ (NoNulo) = False
consultaNo item menu 
                    | cab == item = True
                    | otherwise =  consultaNo item (removeNo cab menu)
            where cab = (infoNo menu)

insereNo :: (Codigo, Nome, Preco) -> Arv ItemRest -> Arv ItemRest
insereNo (c, n, p) NoNulo = (No (c, n, p) NoNulo NoNulo)
insereNo (c, n, p) (No y tesq tdir)
                    | c == getCod y = (No (c, n, p) tesq tdir)
                    | c > getCod y = No y tesq (insereNo (c, n, p) tdir)
                    | otherwise = No y (insereNo (c, n, p) tesq) tdir
                where getCod (c, n, p) = c

removeNo ::  (Codigo, Nome, Preco) -> Arv ItemRest -> Arv ItemRest
removeNo _ (NoNulo) = NoNulo
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
cardapio = No (15, "Agua", 400) (No (2, "Cerveja", 800) NoNulo NoNulo) (No (161, "Pastel", 1000) (No (101, "Picanha", 8850) NoNulo NoNulo) (No (52, "Batata-Frita", 1275) NoNulo NoNulo))

-- RESPOSTAS DA LISTA
-- QUESTÃO 2
-- A) COLETA ITEM PELO COD
coletaItemMenu :: Menu -> Codigo -> ItemRest
coletaItemMenu (NoNulo) _ =  error "ITEM NÃO CONSTA NO MENU"
coletaItemMenu menu c 
                    | getCod cab == c = cab
                    | otherwise = coletaItemMenu (removeNo cab aux) c
                where getCod (c,n,p) = c 
                      cab = infoNo menu
                      aux = menu

-- B) COLETA ITEM PELO NOME
coletaItemMenu2 :: Menu -> Nome-> ItemRest
coletaItemMenu2 (NoNulo) _ =  error "ITEM NÃO CONSTA NO MENU"
coletaItemMenu2 menu n 
                    | getNome cab == n = cab
                    | otherwise = coletaItemMenu2 (removeNo cab aux) n
                where getNome (c,n,p) = n 
                      cab = infoNo menu
                      aux = menu

-- C) ATUALIZA PREÇOS
atualizaPrecosMenu :: Menu -> Int -> Menu
atualizaPrecosMenu (NoNulo) _ = NoNulo
atualizaPrecosMenu (No cb eq dr) a =  (No (up cb) (atualizaPrecosMenu eq a) (atualizaPrecosMenu dr a)) 
    where up (c,n,p) = (c, n, p+(div (a*p) 100))

-- D) ATUALIZA PREÇOS POR CATEGORIA
-- DEFINIÇÃO DOS TIPO DE CATEGORIAS 
data Categoria = Bebidas | TiraGosto | Carnes | Aves | PeixeseMariscos | Massas | Extras | Sobremesas 
                    deriving(Eq, Show, Enum)


atualizaPrecosCat:: Menu -> Categoria -> Int-> Menu
atualizaPrecosCat (NoNulo) _ _ = NoNulo
atualizaPrecosCat (No cb eq dr) cat a = (No (up cb) (atualizaPrecosCat eq cat a) (atualizaPrecosCat dr cat a))
    where getCat = catMaiorMenorCod cat 
          up (c, n, p) = if c >= (fst getCat) && c <= (snd getCat) then (c, n, p+(div (a*p) 100)) else (c,n,p)
          

-- FUNÇÃO AUXILIAR PARA PEGAR INTERVALO DA CATEGORIA
catMaiorMenorCod :: Categoria -> (Codigo,Codigo)
catMaiorMenorCod cat 
                | cat == Bebidas = (1,50)
                | cat == TiraGosto = (51,100)
                | cat == Carnes = (101,120)
                | cat == Aves = (121,140)
                | cat == PeixeseMariscos = (141,160)
                | cat == Massas = (161,180)
                | cat == Extras = (181,200)
                | cat == Sobremesas = (201,220)
                | otherwise = error "CATEGORIA INVÁLIDA!"

-- E) INSERE ITENS NO MENU
insereItemMenu :: Menu ->[ItemRest] -> Menu 
insereItemMenu menu [] = menu
insereItemMenu menu [x] = insereNo x menu
insereItemMenu menu (x:xs) = (insereItemMenu (insereNo x menu) xs)

-- F) REMOVE ITENS DO MENU
removeItemMenu :: Menu ->[ItemRest] -> Menu
--removeItemMenu (NoNulo) _ = NoNulo
removeItemMenu menu [] = menu
removeItemMenu menu [x] = removeNo x menu
removeItemMenu menu (x:xs) =  (removeItemMenu (removeNo x menu) xs)

-- G) COLETA ITENS DE UMA CATEGORIA
coletaItensCat :: Menu ->Categoria -> [ItemRest]
coletaItensCat (NoNulo) _ = []
coletaItensCat menu cat 
                    | verificaCat cab cat = (coletaItensCat (removeNo cab aux) cat)++ [cab]
                    | otherwise = coletaItensCat (removeNo cab aux) cat
                where cab = (infoNo menu)
                      aux = menu

verificaCat :: ItemRest -> Categoria -> Bool
verificaCat (c,n,p) cat
                    | c >= (fst getCat) && c <= (snd getCat) = True
                    | otherwise = False
                where getCat = catMaiorMenorCod cat

-- H) TOTAL DE ITENS NO MENU
totalMenu :: Menu ->Int
totalMenu (NoNulo) = 0
totalMenu menu = 1 + totalMenu (removeNo cab aux)
    where cab = (infoNo menu)
          aux = menu

-- I) TOTAL DE ITENS POR CATEGORIA NO MENU
type NomeCat = String
totalCat:: Menu ->[(NomeCat,Int)]
totalCat (NoNulo) = []
totalCat menu = pega aux menu
    where aux = [Bebidas , TiraGosto , Carnes , Aves , PeixeseMariscos , Massas ,Extras ,Sobremesas]
          pega [] _ = []
          pega (c:cs) menu = (pega cs menu) ++[(show c, count (coletaItensCat menu c))]
          count [] = 0
          count (x:xs) = 1 + count xs

-- J) ITEM MAIS CARO E MAIS BARATO DE UMA CATEGORIA
itemCaroBarat :: Menu -> Categoria -> [(NomeCat,(Nome,Nome))]
itemCaroBarat (NoNulo) _ = []
itemCaroBarat menu cat  = [( show cat,((getNome (min itens), (getNome (max itens)))))]
    where itens = coletaItensCat menu cat
          max [x] = x
          max ((c,n,p):(c1,n1,p1):itens) 
                                    | p >= p1 = max ((c,n,p):itens)
                                    | otherwise = max ((c1,n1,p1):itens)
          min [x] = x
          min ((c,n,p):(c1,n1,p1):itens) 
                                    | p <= p1 = min ((c,n,p):itens)
                                    | otherwise = min ((c1,n1,p1):itens)

          getNome (c,n,p) = n


-- QUESTÃO 3
-- DESCONTO POR DIA DA SEMANA
data DiaSemana = Seg | Ter | Qua | Qui | Sex | Sab | Dom  deriving(Eq, Show, Enum)

desconto :: [(Quant,Nome,Preco)] -> Menu -> DiaSemana -> Int-> [(Quant,Nome,Preco)]
desconto [] _ _ _ = []
desconto (p:ps) menu dia d 
                        | (getCat p) == Bebidas && dia == Seg = [desc p (catMaiorMenorCod (getCat p)) menu d ] ++ (desconto ps menu dia d)
                        | (getCat p)  == PeixeseMariscos && dia == Ter = [desc p (catMaiorMenorCod (getCat p)) menu d ] ++ (desconto ps menu dia d)
                        | (getCat p)  == TiraGosto && dia == Qua = [desc p (catMaiorMenorCod (getCat p)) menu d ] ++ (desconto ps menu dia d)
                        | ((getCat p) == Carnes || (getCat p) == Aves) && dia == Qui = [desc p (catMaiorMenorCod (getCat p)) menu d ] ++ (desconto ps menu dia d)
                        | (getCat p) == Massas && dia == Sex = [desc p (catMaiorMenorCod (getCat p)) menu d ] ++ (desconto ps menu dia d)
                        | otherwise = [p] ++ (desconto ps menu dia d)
                where getCat (q,n,p) = pegaCategoria (coletaItemMenu2 menu n)
                      desc (q,n,p) (x,y) menu d = if (getCod (coletaItemMenu2 menu n)) >= x && (getCod (coletaItemMenu2 menu n)) <= y then (q, n , p-(div (d*p) 100)) else (q, n, p)
                      getNome (c,n,p) = n
                      getCod (c,n,p) = c

-- FUNÇÃO AUXILIAR PRA PEGAR CATEGORIA DE UMA ITEM
pegaCategoria :: ItemRest -> Categoria
pegaCategoria (c,n,p) 
                |  c>= 1 && c <=50 = Bebidas
                |  c>= 51 && c <=100 = TiraGosto
                |  c>= 101 && c <=120 = Carnes 
                |  c>= 121 && c <=140 = Aves 
                |  c>= 141 && c <=160 = PeixeseMariscos 
                |  c>= 161 && c <=180 = Massas 
                |  c>= 181 && c <=200 = Extras 
                |  c>= 201 && c <=220 = Sobremesas
                | otherwise = error "CATEGORIA INVÁLIDA!"