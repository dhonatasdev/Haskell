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


verificaMenu:: Menu -> CodProd -> Int
verificaMenu xs n = foldr(+) 0 ls

   where
   ls = (filter(==n).map(converte)) xs
   converte (cod, nome, preco) = cod

removeProdMenu :: Menu -> CodProd-> Menu
removeProdMenu menu codprod = if (length.filter(\(cod, nome, preco) -> cod ==codprod)) menu == 0
                                  then error "Codigo invalido"
                                  else filter (\(cod, nome, preco) -> cod /= codprod) menu



coletaProdMenu :: Menu -> CodProd -> Produto
coletaProdMenu menu codprod = (head.filter(\(cod, nome, preco) -> cod == codprod)) menu

adicionaCliente :: Clientes -> NomeCliente-> Clientes
adicionaCliente [] nomecliente = [(1, nomecliente, 'A', 0)]
adicionaCliente clientes nomecliente = (1+(converte.head)clientes, nomecliente, 'A', 0):clientes

   where
   converte (cod, nome, cat, cons) = cod

coletaCliente :: Clientes -> CodCliente-> Cliente
coletaCliente clientes codcliente = (head.filter(\(cod, nome, cat, cons) -> cod == codcliente)) clientes

atualizaConsumo :: Clientes -> CodCliente-> Compra-> Clientes
atualizaConsumo clientes codcliente compra = map cond clientes

   where
   cond (cod, nome, cat, cons) =
      if cod == codcliente
         then (cod, nome, cat, cons+compra)
         else (cod, nome, cat, cons)


atualizaClientes :: Clientes -> Clientes
atualizaClientes [] = []
atualizaClientes clientes = map (\(cod, nome, cat, cons) -> (cod, nome, atualizaCat cons, 0)) clientes 


atualizaCat :: ConsumoAnual-> CategCliente
atualizaCat consumo 
       | consumo < 50000 = 'A'
       | consumo >= 50000 && consumo <150000 = 'B'
       | consumo >= 150000 && consumo < 250000 = 'C'
       | consumo >= 250000 && consumo <350000 = 'D'
       | otherwise = 'E'

adicionaProdPedido :: PedidoCliente -> Menu -> CodProd -> Quant -> PedidoCliente
adicionaProdPedido [] menu codprod quant = (codprod, indexaNome codprod menu, quant):[]
adicionaProdPedido pedidocliente menu codprod quant = if existePedido pedidocliente codprod > 0 
                                                           then inclementa pedidocliente codprod quant
                                                           else (codprod, indexaNome codprod menu, quant):pedidocliente

indexaNome :: CodProd -> Menu -> NomeProd
indexaNome codprod menu = (foldr(++) "".map (converte)) (filter(\(cod, nome, valor) -> cod == codprod) menu)
   
    where
    converte (cod, nome, preco) = nome

existePedido :: Menu -> CodProd -> Quant
existePedido lista codprod = length (filter (\(cod, nome, preco) -> cod == codprod) lista)

inclementa:: PedidoCliente -> CodProd -> Quant -> PedidoCliente
inclementa pedidocliente codprod quant = map cond pedidocliente

     where
     cond (cod, nome, qtde)
          | cod == codprod = (cod, nome, qtde+quant)
          | otherwise = (cod, nome, qtde)


cancelaProduto :: PedidoCliente -> CodProd-> Quant-> PedidoCliente
cancelaProduto lista codprod quant= if ((foldr (+) 0.map(converte).filter(\(cod, nome, qtde) -> codprod == cod)) lista) - codprod <= 0
                                        then filter (\(cod, nome, qtde) -> cod /= codprod)lista
                                        else map cond lista
     where
     converte (cod, nome, qtde) = qtde
     cond (cod2, nome2, qtde2) 
          | cod2/= codprod = (cod2, nome2, qtde2)
          | otherwise = (cod2, nome2, qtde2 - quant)

adicionaPedido :: Pedidos -> PedidoCliente -> CodCliente -> Pedidos
adicionaPedido [] pedidocliente codcliente = [] ++ [(codcliente, pedidocliente)]
adicionaPedido pedidos pedidocliente codcliente = pedidos ++ [(codcliente, pedidocliente)]


ordenaMenu :: Menu -> Menu
ordenaMenu [] = []
ordenaMenu menu = foldr ordMenu [] menu

ordMenu :: Produto -> Menu -> Menu
ordMenu (cod, nome, preco) [] = [(cod, nome, preco)]
ordMenu (cod, nome, preco) menu = takeWhile menorz menu ++ [(cod, nome, preco)] ++ dropWhile menorz menu

    where 
    menorz (cod2, nome2, preco2) = cod2 <= cod 

ordenaClientes :: Clientes -> Clientes
ordenaClientes [] = []
ordenaClientes ((cod, nome, cat, cons) : clientes) = (ordenaClientes menores) ++ [(cod, nome, cat, cons)] ++ (ordenaClientes maiores)
    where
        menores = filter (\(cod2, nome2, cat2, cons2) -> nome2 <= nome)clientes
        maiores = filter (\(cond2, nome2, cat2, cons2) -> nome2 > nome)clientes


listaCardapioOrd :: Menu -> IO()
listaCardapioOrd menu = putStr ("CARDAPIO\n\n" ++ ls)

 where
 novoMenu = ordenaMenu menu
 ls = formataLinhas2 novoMenu


formataValor2 :: Preco -> String
formataValor2 valor = (replicate (12 -n) '.') ++ show qtde ++"0"

 where
  qtde = (fromIntegral valor)/100
  n = length (show valor)

formataLinha2 :: Produto -> String
formataLinha2 (codprod, nome, preco) = (replicate (4-p) ' ') ++ show codprod ++ "  "++ nome ++ (replicate (20-n) '.') ++ formataValor2 (preco) ++ "\n"

 where
 n = length (nome)
 p = length (show codprod)

formataLinhas2 :: Menu -> String
formataLinhas2 [] = ""
formataLinhas2 cardapio = (foldr (++) [].map(formataLinha2)) cardapio

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
geraPedidoImpressao pedidocliente menu = map cond pedidocliente

  where
  cond (cod, nome, qtde) = (cod, nome, qtde, (selecionaPreco cod menu * qtde))
  selecionaPreco cod menu = (foldr (+) 0.map(converte).filter(\(codprod, nomeprod, preco) -> codprod == cod))menu 
  converte (cod, nome, preco) = preco


formataLinha :: ProdTotalizado -> String
formataLinha (cod, nome, qtde, preco) = (replicate(4-length (show cod)) ' ') ++ show cod ++ (replicate (5-length (show qtde))' ') ++ show qtde ++ "  " ++ nome ++ (replicate(20-length nome) '.') ++ formataValor(preco) ++ "\n"
     
formataLinhas:: PedidoTotalizado -> String
formataLinhas lista = (foldr (++) [].map (formataLinha)) lista

formataValor :: Int-> String
formataValor valor = (replicate (12 -n) '.') ++ show qtde ++"0"
 where
  qtde = (fromIntegral valor)/ 100
  n = length (show valor)


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
 pt2 = formataLinhas pt1     
 pt4 = totalPedido clientes codcliente (geraPedidoImpressao pedidocliente menu) 
 pt5 = formataTotal pt4

formataTotal :: Totalizacao -> String
formataTotal (total, desconto, pfinal) = "\n           Total............." ++ (replicate (12-tam)'.') ++ (show total1) ++".00"++ "\n" ++ "           Desconto.........." ++ (replicate (12-tam2)'.') ++ show (total1 - (div pfinal 100))++ ".00\n" ++ "           A Pagar............." ++ (replicate (12 - tam3)'.')++show (div pfinal 100) ++ ".00\n"
 where 
 total1 = div total 100
 tam = length (show total1)
 tam2 = length (show desconto)
 tam3 = length (show pfinal)
 descontofinal = (total1 - pfinal) 



incorporaCompra :: Clientes -> CodCliente -> PedidoTotalizado -> Clientes
incorporaCompra [] _ _ = []
incorporaCompra clientes codcliente pedidototalizado = clientela
      
    where
    (preco, desconto, precofinal) = totalPedido clientes codcliente pedidototalizado
    clientela = atualizaConsumo clientes codcliente precofinal 

entregaPedido :: Pedidos -> CodCliente-> Pedidos
entregaPedido pedidos codcliente = filter (\pedido -> fst pedido/= codcliente)pedidos

totalPedido :: Clientes -> CodCliente-> PedidoTotalizado -> Totalizacao
totalPedido clientes codcliente pedidototalizado = (precototal, desconto, precofinal)

  where
  precototal = (foldr (+) 0.map(formata)) pedidototalizado
  desconto = (aplicaDesconto.head.map(formata2).filter(\(cod, nome, cat, cons) -> codcliente == cod))clientes
  precofinal = div ((100-(desconto))*precototal) 100
  formata (codprod, nome, qtde, valor) = valor
  formata2 (cod, nomecliente, cat, cons) = cat
  

selecionaCliente :: Clientes -> CodCliente -> Clientes
selecionaCliente clientes codcliente = filter (\(cod, nome, cat, cons) -> cod == codcliente) clientes

{-colapsaArv :: Arvp a -> [a]
colapsaArv NoNulop = []
colapsaArv (Nop a a1 a2) = colapsaArv a1 ++ [a] ++ colapsaArv a2-}
