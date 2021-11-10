-- Nome: José Dhonatas Alves Sales

type CodProd = Int
type NomeProd = String
type PrecoProd = Int
type Produto = (CodProd, NomeProd, PrecoProd)
type Menu = [Produto]
type CodCliente = Int
type NomeCliente = String
type CategCliente = Char
type ConsumoAnual = Int
type Cliente = (CodCliente, NomeCliente, CategCliente, ConsumoAnual)
type Clientes = [Cliente]
type Quant= Int
type SolCliente = (CodProd, NomeProd, Quant)
type PedidoCliente = [SolCliente]
type Pedidos = [(CodCliente, PedidoCliente)]
type Compra = Int 
type Preco = Int
type ProdTotalizado = (CodProd, NomeProd, Quant, Preco)
type PedidoTotalizado = [ProdTotalizado]
type Categoria = Char



cardapio :: Menu
cardapio = [(150, "Hamburguer", 1000), (15, "Agua", 400), (2, "Coca-cola", 600), (40, "Batata-frita", 850), (52, "Tartelete", 1550)]

fregueses :: Clientes
fregueses = [(4, "Marcos Sa", 'A', 38000), (3, "Mateus Oliveira", 'A', 30000), (2, "Sofia Reis", 'B', 50000), (1, "Paulo Souza", 'C', 100000)]

pedidosRest :: Pedidos
pedidosRest = [(12,[(150,"Hamburguer", 1),(2,"Coca-cola",2)]), (13,[(40,"Batata-frita",4),(15,"Agua",3),(2,"Coca-cola",1)])]

adicionaProdMenu :: Menu -> Produto -> Menu
adicionaProdMenu menu (cod2, nome2, preco2) = if (verificaMenu menu cod2) > 0
                                                   then error "o codigo nao pode ser computado" 
                                                   else (cod2, nome2, preco2):cardapio


verificaMenu :: Menu -> Int -> Int
verificaMenu [] _ = 0
verificaMenu ((cod,nome,preco):menu) codprod 
            | cod== codprod = 1+(verificaMenu menu codprod)
            | otherwise = verificaMenu menu codprod

removeProdMenu :: Menu -> CodProd -> Menu
removeProdMenu menu codprod = if verificaRemover menu codprod > 0
                                then exibeRemocao menu codprod
                                else error "Codigo invalido"

exibeRemocao :: Menu -> CodProd -> Menu
exibeRemocao [] _ = []
exibeRemocao ((cod, nome, preco):menu) codprod 
            | cod == codprod = exibeRemocao menu codprod
            | otherwise = (cod, nome, preco):exibeRemocao menu codprod

verificaRemover :: Menu -> CodProd -> Int
verificaRemover [] _ = 0
verificaRemover ((codprod, nome, preco):menu) cod
              | codprod == cod = 1+ verificaRemover menu cod
              | otherwise = verificaRemover menu cod


coletaProdMenu :: Menu -> CodProd -> [Produto]
coletaProdMenu [] _ = []
coletaProdMenu ((cod, nome, preco):menu) codprod 
             | cod == codprod = (cod, nome, preco): coletaProdMenu menu codprod 
             | otherwise = coletaProdMenu menu codprod 

adicionaCliente :: Clientes -> NomeCliente -> Clientes
adicionaCliente [] nomecliente = [(0, nomecliente, 'A', 0)]
adicionaCliente clientes nomecliente = (x+1, nomecliente, 'A', 0):clientes

 where
  (cod, nome, cat, cons) = head (clientes)
  x = cod

coletaCliente :: Clientes -> CodCliente -> Clientes
coletaCliente clientes codcliente = dadosCliente clientes codcliente

dadosCliente :: Clientes -> CodCliente -> Clientes
dadosCliente [] _ = []
dadosCliente ((cod, nome, cat, cons):clientes) codcliente 
              | cod == codcliente = [(cod, nome, cat, cons)]
              | otherwise = dadosCliente clientes codcliente

atualizaConsumo :: Clientes -> CodCliente -> Compra -> Clientes
atualizaConsumo [] _ _ = []
atualizaConsumo clientes codcliente compra = atualizar clientes codcliente compra


atualizar:: Clientes -> CodCliente -> Compra -> Clientes
atualizar [] _ _ = []
atualizar ((cod, nome, cat, z2):clientes) codcliente compra
       | cod ==  codcliente = (cod, nome, cat, z2 + compra): atualizar clientes codcliente compra
       | otherwise = (cod, nome, cat, z2) : atualizar clientes codcliente compra

atualizaClientes :: Clientes -> Clientes
atualizaClientes [] = []
atualizaClientes ((cod, nome, cat, cons):clientes) = (cod, nome, atualizaCat cons, 0): atualizaClientes clientes 

atualizaCat:: Int -> Char
atualizaCat n = if n < 50000
                  then 'A'
                  else if n >= 50000 && n<150000
                    then 'B'
                    else if n>=150000 && n<250000
                     then 'C'
                     else if n>=250000 && n < 350000
                      then 'D'
                      else 'E'

adicionaProdPedido :: PedidoCliente -> Menu -> CodProd -> Quant -> PedidoCliente
adicionaProdPedido [] menu codprod quant = (codprod, indexanome codprod menu, quant):[]
adicionaProdPedido pedidocliente menu codprod quant = if existePedido pedidocliente codprod == 0 
                                                         then (codprod, indexanome codprod menu, quant):pedidocliente
                                                         else inclementa pedidocliente codprod quant

indexanome:: CodProd -> Menu -> String
indexanome codprod ((cod, nome, preco):menu)
            | codprod == cod = nome
            | otherwise = indexanome codprod menu 

existePedido:: PedidoCliente -> CodProd -> Int
existePedido [] _ = 0
existePedido ((cod, nome, quant):pedidocliente) codprod
            | codprod == cod = 1
            | otherwise = existePedido pedidocliente codprod

inclementa :: PedidoCliente -> CodProd -> Quant -> PedidoCliente
inclementa [] _ _ = []
inclementa ((cod, nome, qtde):pedidocliente) codprod quant 
                | cod == codprod = (cod, nome, qtde + quant):inclementa pedidocliente codprod quant
                | otherwise = (cod, nome, qtde):inclementa pedidocliente codprod quant 

cancelaProduto :: PedidoCliente -> CodProd -> Quant -> PedidoCliente
cancelaProduto [] _ _ = []
cancelaProduto ((cod, nome, qtde):pedidocliente) codprod quant 
          | codprod == cod && qtde - quant <= 0 = cancelaProduto pedidocliente codprod quant
          | codprod == cod && qtde - quant > 0 = (cod, nome, qtde-quant): cancelaProduto pedidocliente codprod quant
          | otherwise = (cod, nome, qtde):cancelaProduto pedidocliente codprod quant

adicionaPedido :: Pedidos -> PedidoCliente -> CodCliente -> Pedidos
adicionaPedido [] pedidocliente codcliente = [] ++ [(codcliente, pedidocliente)]
adicionaPedido pedidos pedidocliente codcliente = pedidos ++ [(codcliente, pedidocliente)]


ordenaMenu :: Menu -> Menu
ordenaMenu [] = []
ordenaMenu ((codprod, nomeprod, precoprod):menu) = (ordenaMenu pt1) ++ [(codprod, nomeprod, precoprod)] ++ (ordenaMenu pt2) 

 where
  pt1 = selecionaParte1 menu codprod 
  pt2 = selecionaParte2 menu codprod 

selecionaParte1 :: Menu -> Int -> Menu
selecionaParte1 [] _ = []
selecionaParte1 ((cod, nome, preco):menu) codprod 
              | cod <= codprod = (cod, nome, preco): selecionaParte1 menu codprod
              | otherwise = selecionaParte1 menu codprod

selecionaParte2 :: Menu -> Int -> Menu
selecionaParte2 [] _ = []
selecionaParte2 ((cod, nome,preco):menu) codprod 
              | cod > codprod = (cod, nome, preco): selecionaParte2 menu codprod 
              | otherwise = selecionaParte2 menu codprod

ordenaClientes:: [(Int, String, Char, Int)] -> [(Int, String, Char, Int)]
ordenaClientes [] = []
ordenaClientes [xs] = [xs]
ordenaClientes xs =  comparaLista (ordenaClientes pt1) (ordenaClientes pt2)
     where
     metade = div (length xs) 2
     pt1 = take metade xs
     pt2 = drop metade xs


comparaLista :: Clientes -> Clientes -> Clientes
comparaLista [] []  = []
comparaLista xs [] = xs
comparaLista [] ys = ys
comparaLista ((cod, nome, cat, cons):xs) ((cod2, nome2, cat2, cons2):ys)
     | nome<=nome2 = (cod, nome, cat, cons): comparaLista xs ((cod2, nome2, cat2, cons2):ys)
     | otherwise = (cod2, nome2, cat2, cons2): comparaLista ((cod, nome, cat, cons):xs) ys

listaCardapioOrd :: Menu -> IO()
listaCardapioOrd menu = putStr ("CARDAPIO\n\n" ++ ls)

 where
 novoMenu = ordenaMenu menu
 ls = formataLinhas2 novoMenu


formataValor2 :: Int -> String
formataValor2 valor = (repeteco (12 -n) '.') ++ show qtde ++"0"

 where
  qtde = (fromIntegral valor)/100
  n = meuLength (show valor)

formataLinha2 :: (Int, String, Int) -> String
formataLinha2 (codprod, nome, preco) = (repeteco (4-p) ' ') ++ show codprod ++ "  "++ nome ++ (repeteco (20-n) '.') ++ formataValor2 (preco) ++ "\n"

 where
 n = meuLength (nome)
 p = meuLength (show codprod)

formataLinhas2 :: Menu -> String
formataLinhas2 [] = ""
formataLinhas2 ((codprod, nome, preco):cardapio) = formataLinha2 (codprod, nome, preco) ++ formataLinhas2 cardapio


listaClientesCatOrd :: Clientes -> Categoria -> IO()
listaClientesCatOrd clien categoria = if verip clientes categoria > 0
                                        then putStr (("CLIENTES\n\n"++ "CATEGORIA" ++ " "++ charz ++"\n\n") ++ ls)
                                        else putStr ("Nao ha clientes para a categoria" ++ charz ++ "\n")
 
 where 
 clientes = ordenaClientes clien
 clientesSeleciona = selecionaClienteCat clientes categoria
 ls = formataCliente clientesSeleciona
 charz = [categoria]

verip :: Clientes -> Categoria -> Int 
verip [] _ = 0
verip ((cod, nome, cat, cons):clientes) categoria
             | cat == categoria = 1+verip clientes categoria
             | otherwise = verip clientes categoria


selecionaClienteCat :: Clientes -> Categoria -> Clientes
selecionaClienteCat [] _ = []
selecionaClienteCat ((cod, nome, cat, cons):clientes) categoria
                | cat == categoria = (cod, nome, cat, cons): selecionaClienteCat clientes categoria
                | otherwise = selecionaClienteCat clientes categoria


formataCliente :: Clientes -> String
formataCliente [] = ""
formataCliente ((cod, nome, cat, cons):cliente) = "    " ++ show cod ++"    " ++ verificaCat (nome, cat) ++ formataCliente cliente 

verificaCat :: (String, Char) -> String
verificaCat (nome, cat)
            |cat == 'A' = nome ++ "\n"
            |cat == 'B' = nome ++ "\n"
            |cat == 'C' = nome ++ "\n"
            |cat == 'D' = nome ++ "\n"                   
            |cat == 'E' = nome ++ "\n"                      
            |otherwise = "Formato invalido\n"


listaClientesOrd :: Clientes -> IO()
listaClientesOrd clien = putStr ("CLIENTES\n\n" ++ pt1 ++ pt2++ pt3 ++ pt4 ++ pt5)
  
  where 
  clientes = ordenaClientes clien
  pt1 = listaClientesCat2 clientes 'A'
  pt2 = listaClientesCat2 clientes 'B'
  pt3 = listaClientesCat2 clientes 'C'
  pt4 = listaClientesCat2 clientes 'D'
  pt5 = listaClientesCat2 clientes 'E'

listaClientesCat2 :: Clientes -> Categoria -> String
listaClientesCat2 clientes categoria = if verip clientes categoria > 0
                                        then ("CATEGORIA" ++" "++ charz ++"\n\n")++ xs ++"\n"
                                        else ("CATEGORIA" ++ " "++ charz ++"\n\n") ++ "Nao ha clientes para a categoria" ++" "++ charz ++ "\n\n"

 where 
 ls = selecionaClienteCat clientes categoria
 xs = formataCliente ls
 charz = [categoria]


geraPedidoImpressao :: PedidoCliente -> Menu -> PedidoTotalizado
geraPedidoImpressao [] menu = []
geraPedidoImpressao ((cod, nome, qtde):pedidocliente) menu = [(cod, nome, qtde, fazMulti cod qtde menu)] ++ geraPedidoImpressao pedidocliente menu

fazMulti:: Int -> Int -> [(Int, String, Int)] -> Int
fazMulti _ _ [] = 1
fazMulti a b ((cod, nome, preco):menu) 
        | a == cod = b*preco* fazMulti a b menu
        | otherwise = fazMulti a b menu

formataGeral :: PedidoTotalizado -> String
formataGeral [] = ""
formataGeral ((codprod, nome, qtde, preco):pedidototalizado) = tudoJunto ++ formataGeral pedidototalizado -- juncao do formataLinhas e formataLinha

 where
 n = meuLength nome
 p = meuLength(show codprod)
 o = meuLength(show qtde)  
 tudoJunto = (repeteco (4-p)' ') ++ show codprod ++ (repeteco (5-o)' ') ++ show qtde ++"  " ++ nome ++ (repeteco (20-n) '.') ++ formataValor (preco) ++"\n"


repeteco:: Int -> Char -> String
repeteco 0 _ = ""
repeteco n x = [x] ++ repeteco (n-1) x

meuLength:: String -> Int
meuLength [] = 0
meuLength (x:xs) = 1+meuLength xs

formataValor :: Int-> String
formataValor valor = (repeteco (12 -n) '.') ++ show qtde ++"0"
 where
  qtde = (fromIntegral valor)/ 100
  n = meuLength (show valor)


type Desconto = Int
type PrecoFinal = Int
type Totalizacao = (Preco, Desconto, PrecoFinal)


aplicaDesconto:: Char -> Int
aplicaDesconto n 
                 | n == 'A' = 0
                 | n == 'B' = 3
                 | n == 'C' = 5
                 | n == 'D' = 10
                 | otherwise = 15


geraConta:: CodCliente -> Clientes-> Menu -> PedidoCliente -> IO ()
geraConta codcliente clientes menu pedidocliente = putStr ("Pedido "++ show codcliente ++"\n" ++ "COD   QTD  PRODUTO---------------------PRECO\n\n"++ pt2 ++ pt5)

 where 
 pt1 = geraPedidoImpressao pedidocliente menu 
 pt2 = formataGeral pt1
 pt4 = totalPedido clientes codcliente (geraPedidoImpressao pedidocliente menu) 
 pt5 = formataTotal pt4

formataTotal :: Totalizacao -> String
formataTotal (total, desconto, pfinal) = "\n           Total............." ++ (repeteco (12-tam)'.') ++ (show total1) ++".00"++ "\n" ++ "           Desconto.........." ++ (repeteco (12-tam2)'.') ++ show (total1 - (div pfinal 100))++ ".00\n" ++ "           A Pagar............." ++ (repeteco (12 - tam3)'.')++show (div pfinal 100) ++ ".00\n"
 where 
 total1 = div total 100
 tam = meuLength (show total1)
 tam2 = meuLength (show desconto)
 tam3 = meuLength(show pfinal)
 descontofinal = (total1 - pfinal) 



incorporaCompra :: Clientes -> CodCliente -> PedidoTotalizado -> Clientes
incorporaCompra [] _ _ = []
incorporaCompra clientes codcliente pedidototalizado = clientela
      
    where
    (preco, desconto, precofinal) = totalPedido clientes codcliente pedidototalizado
    clientela = atualizaConsumo clientes codcliente precofinal 

entregaPedido :: Pedidos -> CodCliente -> Pedidos
entregaPedido [] codcliente = []
entregaPedido ((x,((a,b,c):ys)):xs) codcliente
               | x /= codcliente = (x,((a,b,c):ys)): entregaPedido xs codcliente
               | otherwise = entregaPedido xs codcliente



totalPedido:: Clientes -> CodCliente -> PedidoTotalizado -> Totalizacao
totalPedido clientes codcliente pedidototalizado = (total, desconto, precofinal)
   
  where
  total = meuSum pedidototalizado
  [(cod, nome, cat, cons)] = selecionaCliente clientes codcliente
  desconto = aplicaDesconto cat
  precofinal = div ((100 - (desconto))*total) 100
  

selecionaCliente :: Clientes -> CodCliente -> Clientes
selecionaCliente [] _ = []
selecionaCliente ((cod, nome, cat, cons):clientes) codcliente
                | cod == codcliente = (cod, nome, cat, cons): selecionaCliente clientes codcliente
                | otherwise = selecionaCliente clientes codcliente



meuSum :: PedidoTotalizado -> Int 
meuSum [] = 0
meuSum ((cod, nome, qtde, preco):pedidototalizado) = preco + meuSum pedidototalizado 
