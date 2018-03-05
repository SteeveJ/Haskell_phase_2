                
--Question 1 type pour arbre binaire

data Arbre = Noeud Integer (Arbre)(Arbre) | Vide deriving Show

exemple = Noeud 9 
                 (Noeud 5 
                         (Noeud 2 Vide Vide)
                         (Noeud 6 Vide 
                                       (Noeud 8 Vide Vide))) 
                 (Noeud 13 
                          (Noeud 10 Vide Vide)
                          (Noeud 20 Vide Vide))

exemple2 = Noeud 99
                   (Noeud 55 
                            (Noeud 33 
                                     (Noeud 29 
                                              Vide 
                                              (Noeud 31 
                                                       Vide 
                                                       Vide)) 
                                     (Noeud 41 
                                              (Noeud 35 
                                                       Vide 
                                                       Vide) 
                                              (Noeud 48 
                                                       Vide 
                                                       Vide))) 
                            (Noeud 89 
                                     Vide 
                                     Vide))
                   (Noeud 100 
                            (Noeud 161 
                                      Vide 
                                      Vide) 
                            (Noeud 381 
                                      (Noeud 250 Vide Vide) 
                                      Vide))

--Question 2 recherche noeud

recherche :: Integer -> Arbre -> Bool 
recherche _ Vide = False
recherche x (Noeud r g d) | x == r = True | x < r = recherche x g | x > r = recherche x d

--Question 3 supprimer noeud

supprimerPetitDroite :: Arbre -> (Arbre, Integer)
supprimerPetitDroite (Noeud r Vide d) = (d, r)
supprimerPetitDroite (Noeud r g d) =  let(g1, p) = supprimerPetitDroite(g) in (Noeud r g1 d, p)

supprimerNoeud :: Arbre -> Arbre
supprimerNoeud (Noeud _ Vide Vide) = Vide
supprimerNoeud (Noeud _ g Vide) = g
supprimerNoeud (Noeud _ Vide d) = d
supprimerNoeud (Noeud _ g d) = Noeud r1 g d1 where(d1, r1) = supprimerPetitDroite(d)

supprimer :: Integer -> Arbre -> Arbre
supprimer _ Vide = Vide
supprimer e (Noeud r g d) | e == r = supprimerNoeud (Noeud r g d)
                          | e < r = Noeud r (supprimer e g) d
                          | e > r = Noeud r g (supprimer e d)

--Question 4 insertion noeud

inserer :: Integer -> Arbre -> Arbre
inserer x Vide = Noeud x Vide Vide
inserer x (Noeud r g d) | x == r = Noeud r g d
                        | x < r = Noeud r (inserer x g) d
                        | x > r = Noeud r g (inserer x d)

--Question 5 incrementer

incrementer :: Integer -> Arbre -> Arbre
incrementer x Vide = Vide
incrementer x (Noeud r g d) = Noeud (r+x) (incrementer x g) (incrementer x d)

--Question 6 multiplier par 2

multiplier :: Arbre -> Arbre 
multiplier Vide = Vide 
multiplier (Noeud r g d) = Noeud (r*2) (multiplier g) (multiplier d)

--Question 7 inverser

inverser :: Arbre -> Arbre
inverser Vide = Vide
inverser (Noeud r g d) = Noeud (r*(-1)) (inverser d) (inverser g)

--Question 8 fusionner

fusionner :: Arbre -> Arbre -> Arbre
fusionner Vide Vide = Vide
fusionner Vide (Noeud r g d) = (Noeud r g d)
fusionner (Noeud r g d) Vide = (Noeud r g d)
fusionner (Noeud r g d) (Noeud r1 g1 d1) = fusionner (fusionner ( inserer r1 (Noeud r g d)) g1) d1
