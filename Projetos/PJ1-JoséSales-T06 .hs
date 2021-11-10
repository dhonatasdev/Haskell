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
type Cliente = (CodCliente, NomeCliente, CategCliente,
 ConsumoAnual)
type Clientes = [Cliente]
type Quant= Int
type SolCliente = (CodProd, NomeProd, Quant)
type PedidoCliente = [SolCliente]
type Pedidos = [(CodCliente, PedidoCliente)]

cardapio :: Menu
cardapio = [(150, "Hamburguer", 1000), (15, "Agua", 400), (2, "Coca-cola", 600), (40, "Batata-frita", 850), (52, "Tartelete", 1550)]

fregueses :: Clientes
fregueses = [(4, "Marcos Sa", 'A', 38000), (3, "Mateus Oliveira", 'A', 30000), (2, "Sofia Reis", 'B', 50000), (1, "Paulo Souza", 'C', 100000)]

pedidosRest :: Pedidos -> Pedidos
pedidosRest pedidos = pedidos

adicionaProdMenu :: Menu -> Produto -> Menu
adicionaProdMenu menu (cod2, nome2, preco2) = if (length [(cod, nome, preco) | (cod, nome, preco)<-cardapio, cod == cod2] > 0)
                                                       then error "o codigo nao pode ser computado" 
                                                       else (cod2, nome2, preco2):cardapio

removeProdMenu :: Menu -> CodProd -> Menu
removeProdMenu menu codprod = [(cod, nome, preco) | (cod, nome, preco) <-menu, cod /=codprod]

coletaProdMenu :: Menu -> CodProd -> [Produto]
coletaProdMenu menu codprod = [(cod, nome, preco) | (cod, nome, preco) <- menu, cod == codprod]

adicionaCliente :: Clientes -> NomeCliente -> Clientes
adicionaCliente [] nomecliente = [(0, nomecliente, 'A', 0)]
adicionaCliente clientes nomecliente = (x+1, nomecliente, 'A', 0):clientes

 where
  (cod, nome, cat, cons) = head (clientes)
  x = cod

coletaCliente :: Clientes -> CodCliente -> Clientes
coletaCliente clientes codcliente = [(cod, nome, cat, cons) | (cod, nome, cat, cons)<-clientes, cod == codcliente]


type Compra = Int
atualizaConsumo :: Clientes -> CodCliente -> Compra -> Clientes
atualizaConsumo clientes codcliente compra = [(cod, nome, cat, compra)| (cod, nome, cat, cons) <-clientes, cod == codcliente]


atualizaClientes :: Clientes -> Clientes
atualizaClientes clientes = [(cod, nome, atualiza cons, 0)| (cod, nome, cat, cons)<-clientes]

atualiza :: Int -> Char
atualiza n = if n < 50000
               then 'A'
               else if n >= 50000 && n<150000
                then 'B'
                else if n>=150000 && n<250000
                  then 'C'
                  else if n>=250000 && n < 350000
                    then 'D'
                    else 'E'

adicionaProPedido :: PedidoCliente -> Menu -> CodProd -> Quant -> PedidoCliente
adicionaProPedido [] menu codprod quant = (codprod, indexanome codprod menu, quant):[]
adicionaProPedido pedidocliente menu codprod quant = (codprod, nome, quant):pedidocliente

 where
   [(cod, name, qtde)] = [(codi, nomi, quan)| (codi, nomi, quan)<-menu, codi == codprod]
   nome = name

indexanome:: CodProd -> Menu -> String
indexanome codprod menu = nome
 where
   [(cod, name, qtde)] = [(codi, nomi, quan)| (codi, nomi, quan)<-menu, codi == codprod]
   nome = name

cancelaProduto :: PedidoCliente -> CodProd -> Quant -> PedidoCliente
cancelaProduto pedidocliente codprod quant = if verifica > 0
                                                then pt1 ++ pt2
                                                else pt2
   where
   quant2 = [qtde| (cod, nome, qtde)<-pedidocliente, codprod == cod]
   soma = sum quant2
   verifica = soma-quant
   pt1 = [(cod, nome, verifica)|(cod, nome, qtde)<-pedidocliente, codprod == cod]
   pt2 = [(cod, nome, qtde)|(cod, nome, qtde)<-pedidocliente, codprod/=cod]

adicionaPedido :: Pedidos -> PedidoCliente -> CodCliente -> Pedidos
adicionaPedido pedidos pedidocliente codcliente = [(codcliente,[(codprod, nome, qtde)])| (codprod, nome, qtde) <-pedidocliente]

type Preco = Int
type ProdTotalizado = (CodProd, NomeProd, Quant, Preco)
type PedidoTotalizado = [ProdTotalizado]

geraPedidoImpressao :: PedidoCliente -> Menu -> PedidoTotalizado 
geraPedidoImpressao pedidocliente menu = [(cod, nome, qtde, preco*qtde)| (cod, nome, qtde)<-pedidocliente, (cod2, nome2, preco)<-menu, cod2 == cod]

type Desconto = Float
type PrecoFinal = Float
type Totalizacao = (Preco, Desconto, PrecoFinal)

totalPedido :: Clientes -> CodCliente -> PedidoTotalizado -> Totalizacao
totalPedido clientes codcliente pedidototalizado = (preco2, desconto2/100, precofinal/100)

 where
  preco2 = sum[preco| (cod, nome, qtde, preco)<-pedidototalizado, preco>0]
  [(cod, nome, cat, cons)] = [(cod2, nome2, cat2, cons2)| (cod2, nome2, cat2, cons2)<- clientes, cod2 == codcliente]
  desconto = fromIntegral (aplicaDesconto cat)
  precofinal = (100.0-(desconto))*fromIntegral(preco2)
  desconto2 = (desconto*(fromIntegral preco2))/100

aplicaDesconto:: Char -> Int
aplicaDesconto n 
                 | n == 'A' = 0
                 | n == 'B' = 3
                 | n == 'C' = 5
                 | n == 'D' = 10
                 | otherwise = 15


entregaPedido :: Pedidos -> CodCliente -> Pedidos
entregaPedido pedidos codcliente = [(codcliente, [(cod1, nome, qtde)])| (cod,[(cod1, nome, qtde)])<-pedidos, codcliente /= cod]

formataValor :: Int-> String
formataValor valor = (replicate (12 -n) '.') ++ show qtde ++"0"
 where
  qtde = (fromIntegral valor)/ 100
  n = length (show valor)

formataLinhas :: PedidoTotalizado -> String 
formataLinhas [(codprod, nome, qtde, preco)] = (replicate (4-p)' ') ++ show codprod ++ (replicate (5-o)' ') ++ show qtde ++"  " ++ nome ++ (replicate (20-n) '.') ++ formataValor (preco) ++"\n"
 where
 n = length (nome)
 p = length (show codprod)
 o = length (show qtde)   

formataTotal :: Totalizacao -> String
formataTotal (total, desconto, pfinal) = "\n           Total..............." ++ (replicate (12-tam)'.') ++ show (total1) ++ "0\n" ++ "           Desconto............" ++ (replicate (12-tam2)'.') ++ show desconto ++"0"++ "\n" ++ "           A Pagar..............." ++ (replicate (12 - tam3)'.')++show (pfinal/100) ++"0" ++ "\n"

 where 
 total1 = (fromIntegral total)/100
 tam = length (show total1)
 tam2 = length (show desconto)
 tam3 = length(show pfinal)

geraConta:: CodCliente -> Clientes-> Menu -> PedidoCliente -> IO ()
geraConta codcliente clientes menu pedidocliente = putStr ("Pedido "++ show codcliente ++"\n" ++ "COD   QTD  PRODUTO---------------------PRECO\n\n"++ pt3 ++ pt5)

 where 
 pt1 = geraPedidoImpressao pedidocliente menu 
 pt2 = [formataLinhas [(codprod, nome, qtde, preco)]| (codprod, nome, qtde, preco)<-pt1]
 pt3 = concat pt2
 pt4 = totalPedido clientes codcliente (geraPedidoImpressao pedidocliente menu) 
 pt5 = formataTotal pt4

listaCardapio :: Menu -> IO()
listaCardapio menu = putStr ("CARDAPIO\n\n" ++ xs)

 where
 ls = [formataLinha2 (cod, nome, preco)| (cod, nome, preco)<-menu]
 xs = concat ls
  


formataValor2 :: Int -> String
formataValor2 valor = (replicate (12 -n) '.') ++ show qtde ++"0"

 where
  qtde = (fromIntegral valor)/100
  n = length (show valor)

formataLinha2 :: (Int, String, Int) -> String
formataLinha2 (codprod, nome, preco) = (replicate (4-p) ' ') ++ show codprod ++"  "++ nome ++ (replicate (20-n) '.') ++ formataValor2 (preco) ++"\n"

 where
 n = length (nome)
 p = length (show codprod)

type Categoria = Char

listaClientesCat :: Clientes -> Categoria -> IO()
listaClientesCat clientes categoria = if verip > 0
                                        then putStr (("CLIENTES\n\n"++ "CATEGORIA" ++ " "++ charz ++"\n\n") ++ xs)
                                        else putStr ("Nao ha clientes para a categoria" ++ charz ++ "\n")
  
 where 
 verip = length [(cod, nome, cat, cons) | (cod, nome, cat, cons)<-clientes, categoria == cat]
 ls = [formataCliente (cod, nome, cat)| (cod, nome, cat, cons)<-clientes, categoria == cat]
 xs = concat ls
 charz = [categoria]

verificaCat :: (String, Char) -> String
verificaCat (nome, cat)
            |cat == 'A' = nome ++ "\n"
            |cat == 'B' = nome ++ "\n"
            |cat == 'C' = nome ++ "\n"
            |cat == 'D' = nome ++ "\n"                   
            |cat == 'E' = nome ++ "\n"                      
            |otherwise = "Formato invalido\n"

formataCliente :: (Int, String, Char) -> String
formataCliente (cod, nome, cat) = "    " ++ show cod ++"    " ++ verificaCat (nome, cat) 

listaClientes :: Clientes -> IO()
listaClientes clientes = putStr ("CLIENTES\n\n" ++ pt1 ++ pt2++ pt3 ++ pt4 ++ pt5)
  
  where 
  pt1 = listaClientesCat2 clientes 'A'
  pt2 = listaClientesCat2 clientes 'B'
  pt3 = listaClientesCat2 clientes 'C'
  pt4 = listaClientesCat2 clientes 'D'
  pt5 = listaClientesCat2 clientes 'E'

listaClientesCat2 :: Clientes -> Categoria -> String
listaClientesCat2 clientes categoria = if verip > 0
                                        then ("CATEGORIA" ++" "++ charz ++"\n\n")++ xs ++"\n"
                                        else ("CATEGORIA" ++ " "++ charz ++"\n\n") ++ "Nao ha clientes para a categoria" ++" "++ charz ++ "\n\n"

 where 
 verip = length [(cod, nome, cat, cons) | (cod, nome, cat, cons)<-clientes, categoria == cat]
 ls = [formataCliente (cod, nome, cat)| (cod, nome, cat, cons)<-clientes, categoria == cat]
 xs = concat ls
 charz = [categoria]
  
