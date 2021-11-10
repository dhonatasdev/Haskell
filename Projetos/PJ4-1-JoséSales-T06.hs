-- Nome: José Dhonatas Alves Sales

module Teste (arvVazia, ehVazia, ehNoNulo, arvEsq, arvDir, insereNo, coletaNo, infoNo, atualizaNo, consultaNo, mapArv, Arv, removeNo, una, minArv) where 

data Arv a b c d e = NoNulo 
              | No (a, b, c, d, e) (Arv a b c d e) ( Arv a b c d e) deriving Show

arvVazia :: Arv a b c d e
arvVazia = NoNulo

ehVazia :: Arv a b c d e -> Bool
ehVazia (NoNulo) = True
ehVazia _ = False

ehNoNulo :: Arv a b c d e -> Bool
ehNoNulo NoNulo = True
ehNoNulo _ = False

arvEsq :: Arv a b c d e -> Arv a b c d e
arvEsq (NoNulo) = error "nao tem subarvore esquerda"
arvEsq (No _ tesq _) = tesq

arvDir :: Arv a b c d e-> Arv a b c d e
arvDir (NoNulo) = error "nao tem subarvore direita"
arvDir (No _ _ tdir) = tdir


infoNo :: Arv a b c d e -> (a,b,c,d,e)
infoNo NoNulo = error "arvore vazia"
infoNo (No x _ _ ) = x
 
insereNo :: Ord a => (a, b, c, d, e) -> Arv a b c d e -> Arv a b c d e
insereNo (m,n,o,p,q) NoNulo = (No (m, n, o, p, q) NoNulo NoNulo)
insereNo (m,n,o,p,q) (No (r,s,t,u,v) tesq tdir)
         | m == r = (No (r,s,t,u,v) tesq tdir)
         | m < r = No (r,s,t,u,v) (insereNo (m,n,o,p,q) tesq) tdir
         | otherwise = No (r,s,t,u,v) tesq (insereNo (m, n, o, p,q) tdir)


consultaNo :: Ord a => (a, b, c, d, e) -> Arv a b c d e -> Bool
consultaNo _ NoNulo = False
consultaNo (m, n, o, p,q) (No (r,s,t,u,v) tesq tdir) 
         | m == r = True
         | m < r = consultaNo (m, n, o, p, q) tesq
         | otherwise = consultaNo (m, n, o, p,q) tdir

coletaNo :: Ord a => a -> Arv a b c d e -> (a,b,c,d,e)
coletaNo m NoNulo = error "vazio"
coletaNo m (No (r,s,t,u,v) tesq tdir)
        | m == r = (r,s, t,u,v)
        | m < r = coletaNo m tesq
        | otherwise = coletaNo m tdir

atualizaNo :: Ord a => (a, b, c, d, e) -> Arv a b c d e -> Arv a b c d e
atualizaNo (m, n, o, p, q) NoNulo = error "vazio"
atualizaNo (m, n, o, p, q) (No (r,s,t,u,v) tesq tdir)
           | m == r = (No (m, n, o, p,q) tesq tdir)
           | m < r = (No (r,s,t,u,v) (atualizaNo (m, n, o, p,q) tesq) tdir)
           | otherwise = (No (r,s,t,u,v) tesq (atualizaNo (m, n, o, p, q) tdir))



mapArv :: ((a,b,c,d,e) -> (r,s,t,u,v)) -> Arv a b c d e -> Arv r s t u v
mapArv f NoNulo = NoNulo 
mapArv f (No q@(a,b,c,d,e) tesq tdir) = No (f q) (mapArv f tesq) (mapArv f tdir)

removeNo :: Ord a => (a, b, c, d, e) -> Arv a b c d e -> Arv a b c d e
removeNo (x, x2, x3, x4, x5) NoNulo = error "nao há elementos a remover"
removeNo (x, x2, x3, x4, x5) (No (y,z, z2, z3, z4) tesq tdir)
     | x < y = No (y,z, z2, z3, z4) (removeNo (x, x2, x3, x4, x5) tesq) tdir
     | x > y = No (y,z, z2, z3, z4) tesq (removeNo (x, x2, x3, x4, x5) tdir)
     | ehNoNulo tdir = tesq
     | ehNoNulo tesq = tdir
     | otherwise = una tesq tdir
 
 
 
una :: Ord a => Arv a b c d e -> Arv a b c d e-> Arv a b c d e
una tesq tdir = No mini tesq novaArv
   where (Just mini) = minArv tdir
         novaArv = removeNo mini tdir
 
minArv :: Ord a => Arv a b c d e -> Maybe (a, b, c, d, e)
minArv t
     | ehNoNulo t = Nothing
     | ehNoNulo (arvEsq t) = Just (infoNo t)
     | otherwise = minArv (arvEsq t)
 
 

