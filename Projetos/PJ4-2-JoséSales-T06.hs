-- Nome: José Dhonatas Alves Sales

import Teste
import Data.List

type CodProd = Int
type NomeProd = String
type PrecoProd = Int
type Produto = (CodProd, NomeProd, PrecoProd)
type Menu = [Produto]
type CodCliente = Int
type NomeCliente = String
type CategCliente = Char
type ConsumoAnual = Int
type Quant= Int
type SolCliente = (CodProd, NomeProd, Quant)
type PedidoCliente = [SolCliente]
type Pedidos = [(CodCliente, PedidoCliente)]
type Compra = Int 
type Preco = Int
type ProdTotalizado = (CodProd, NomeProd, Quant, Preco)
type PedidoTotalizado = [ProdTotalizado]
type Categoria = Char
type Totalizacao = (Preco, Desconto, PrecoFinal)
type PrecoFinal = Int
type Desconto = Int
type Clientes = Arv CodCliente NomeCliente CategCliente MesAniversario ConsumoAnual
type Cliente = (CodCliente, NomeCliente, CategCliente, MesAniversario, ConsumoAnual)
type MesAniversario = Meses

data Meses = Jan | Fev | Mar | Abri | Maio | Jun | Jul | Ago | Set | Out | Nov | Dez deriving (Show, Eq, Ord, Enum, Read)

cardapio :: Menu
cardapio = [(150, "Hamburguer", 1000), (15, "Agua", 400), (2, "Coca-cola", 600), (40, "Batata-frita", 850), (52, "Tartelete", 1550)]

adicionaCliente:: Clientes -> CodCliente -> NomeCliente -> MesAniversario -> Clientes
adicionaCliente clientes codcliente nomecliente mesaniversario = insereNo (codcliente, nomecliente, 'A', mesaniversario, 0) clientes

coletaCliente :: Clientes -> CodCliente -> Cliente
coletaCliente clientes codcliente = coletaNo codcliente clientes

atualizaConsumo :: Clientes -> CodCliente -> Compra -> Clientes
atualizaConsumo clientes codcliente compra = ls
   where
   (cod, nomecliente, categcliente, mesaniversario, consumoanual) = coletaCliente clientes codcliente
   ls = atualizaNo (cod, nomecliente, categcliente, mesaniversario, consumoanual+compra) clientes

atualizaClientes :: Clientes -> Clientes
atualizaClientes clientes = xs
   where
   lista = transformaLista clientes 
   ls = map(\(cod, nome, cat, mesaniversario, cons) -> (cod, nome, atualizaCat cons, mesaniversario, 0)) lista
   xs = listaArv ls

totalPedido :: Clientes -> CodCliente-> PedidoTotalizado -> Totalizacao
totalPedido clientez codcliente pedidototalizado = (precototal, desconto, precofinal)

  where
  clientes = transformaLista clientez
  precototal = (foldr (+) 0.map(formata)) pedidototalizado
  desconto = (aplicaDesconto.head.map(formata2).filter(\(cod, nome, cat,ani, cons) -> codcliente == cod))clientes
  precofinal = div ((100-(desconto))*precototal) 100
  formata (codprod, nome, qtde, valor) = valor
  formata2 (cod, nomecliente, cat, ani, cons) = cat

geraConta:: CodCliente -> Clientes-> Menu -> PedidoCliente -> IO ()
geraConta codcliente clientes menu pedidocliente = putStr ("Pedido "++ show codcliente ++"\n" ++ "COD   QTD  PRODUTO---------------------PRECO\n\n"++ pt2 ++ pt5)

 where
 clientesLista = transformaLista clientes 
 pt1 = geraPedidoImpressao pedidocliente menu 
 pt2 = formataLinhas pt1     
 pt4 = totalPedido clientes codcliente (geraPedidoImpressao pedidocliente menu) 
 pt5 = formataTotal pt4

transformaLista :: Clientes -> [Cliente]
transformaLista clientes = transformaLista tesq ++ [tupla] ++ transformaLista tdir

    where
    tesq = arvEsq clientes
    tupla = infoNo clientes
    tdir = arvDir clientes

geraListaClienMes :: Clientes -> MesAniversario -> [String]
geraListaClienMes clientes mesaniversario = sort (map (\(codi, name, cate, aniv, consu) -> name)clienteZ)
    where
    clientesLista = transformaLista clientes
    clienteZ = filter(\(cod, nome, cat, ani, cons)-> ani == mesaniversario) clientesLista


meses:: Clientes -> ([MesAniversario], [MesAniversario])
meses clientes = (filter(\m -> elem m meZ)mes, filter(\m -> notElem m meZ)mes)
     
   where
   clienteZ = transformaLista clientes
   mes = map (\(cod, nome, cat, aniver, cons) -> aniver)clienteZ
   meZ = [Jan .. Dez]


listaArv :: [Cliente] -> Clientes
listaArv lista = foldr insereNo arvVazia lista  

atualizaCat :: ConsumoAnual -> CategCliente
atualizaCat consumo 
       | consumo < 50000 = 'A'
       | consumo >= 50000 && consumo <150000 = 'B'
       | consumo >= 150000 && consumo < 250000 = 'C'
       | consumo >= 250000 && consumo <350000 = 'D'
       | otherwise = 'E'


-- o resto nao tem novidade, segue o mesmo padrao da parte 3 do projeto

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



formataTotal :: Totalizacao -> String
formataTotal (total, desconto, pfinal) = "\n           Total............." ++ (replicate (12-tam)'.') ++ (show total1) ++".00"++ "\n" ++ "           Desconto.........." ++ (replicate (12-tam2)'.') ++ show (total1 - (div pfinal 100))++ ".00\n" ++ "           A Pagar............." ++ (replicate (12 - tam3)'.')++show (div pfinal 100) ++ ".00\n"
 where 
 total1 = div total 100
 tam = length (show total1)
 tam2 = length (show desconto)
 tam3 = length (show pfinal)
 descontofinal = (total1 - pfinal) 



aplicaDesconto:: Char -> Int
aplicaDesconto n 
                 | n == 'A' = 0
                 | n == 'B' = 3
                 | n == 'C' = 5
                 | n == 'D' = 10
                 | otherwise = 15


  


