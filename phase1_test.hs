---question 01 ------
{-|      -}
data Couleur = R | N deriving (Show,Eq)
data ABR = Videabr | ABR ABR Int ABR deriving (Show, Eq)
data ARN = Vide | Noeud Couleur (ARN) Int (ARN) deriving (Show,Eq)


---------------question 02 // la fonction de recherche-----------------
{-|      -}
{-
rechercheElement :: Int -> ARN -> (Bool)
rechercheElement x Vide = (False)
rechercheElement x (Noeud gauche r c droite) = if r == x then (True)
                  else
                          if x > r then
                               rechercheElement x droite
                          else
                               rechercheElement x gauche
-}
-----question 02.2 // la fonction de recherche avec la Couleur----------
{-|      -}
{-
rechercheElementcl :: Int ->Couleur-> ARN -> Bool
rechercheElementcl x c Vide = False
rechercheElementcl x c (Noeud gauche r cl droite) = if r == x && cl==c then True
                     else
                          if x > r then
                               rechercheElementcl x c droite
                          else
                               rechercheElementcl x c gauche

-}
 ---question 03  // la fonction d’insertion   -------------------
{-|      -}

balance :: Couleur -> ARN -> Int -> ARN -> ARN
balance N (Noeud R (Noeud R a x b) y c) z d = Noeud R (Noeud N a x b) y (Noeud N c z d)
balance N (Noeud R a x (Noeud R b y c)) z d = Noeud R (Noeud N a x b) y (Noeud N c z d)
balance N a x (Noeud R (Noeud R b y c) z d) = Noeud R (Noeud N a x b) y (Noeud N c z d)
balance N a x (Noeud R b y (Noeud R c z d)) = Noeud R (Noeud N a x b) y (Noeud N c z d)
balance col a x b = Noeud col a x b



insert :: Int-> ARN-> ARN
insert x s = makeBlack $ ins s
  where ins Vide  = Noeud R Vide x Vide
        ins (Noeud col a y b)
          | x < y  = balance col (ins a) y b
          | x == y = Noeud col a y b
          | x > y  = balance col a y (ins b)


{-Q5 : ABR to ARN-}
transformerABR :: ABR->ARN
transformerABR (ABR (ABR fg2 val2 fd2) val (ABR fg3 val3 fd3))=insert val3 (insert val2 (insert val (Vide)))

makeBlack :: ARN->ARN
makeBlack (Noeud col fg val fd)=(Noeud N fg val fd)