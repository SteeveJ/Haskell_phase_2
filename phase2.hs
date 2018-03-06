data ArbreRN = Noeud Int Color (ArbreRN)(ArbreRN) | Vide deriving Show
data Color = R | N deriving Show

exemple = Noeud 20 R (Noeud 14 N (Noeud 15 R Vide Vide) (Noeud 13 R Vide Vide)) (Noeud 500 N Vide Vide)

recherche :: Int -> ArbreRN -> Bool
recherche _ Vide = False
recherche x (Noeud r _ g d) | x == r = True | x < r = recherche x g | x > r = recherche x d
