Objectif du TP : résoudre le jeu des allumettes grâce au parcours d'un ensemble d'états.

1) On choisit la représentation suivante pour les états : (Nombre d'allumettes restantes / Prochain joueur à jouer)
On suppose que le joueur 0 commence et que le jeu se joue avec 11 allumettes iniiales.
Liste des états possibles pour 11 allumettes (le joueur 0 commence):
(11 0)
(10 1)
(9 1)
(8 1)
(9 0)
(8 0)
(7 0)
(6 0)
(5 0)
(7 1)
(6 1)
(5 1)
(4 1)
(3 1)
(2 1)
(4 0)
(3 0)
(2 0)
(1 0)
(0 0)
(1 1)
(0 1)

2) Liste des actions possibles sous la forme (Joueur / Nombre d'alumettes prélevées) :
(1 1)
(1 2)
(1 3)
(0 1)
(0 2)
(0 3)

Actions possibles selon l'etat:
(x 0) -> (0 1) (0 2) (0 3) avec x >= 3
(2 0) -> (0 1) (0 2)
(1 0) -> (0 1)

(x 1) -> (1 1) (1 2) (1 3) avec x >= 3
(2 1) -> (1 1) (1 2)
(1 1) -> (1 1)

(0 0) aucune action possible
(1 0) aucune action possible 

3) Sachant que le joueur 0 commence l'état initial est :
(11 0)

Etats finals :
(0 0) -> il ne reste plus d'allumettes : le joueur 1 a gagné
(0 1) -> il ne reste plus d'allumettes : le joueur 0 a gagné 

Arbre de recherche:
(11 0)
(10 1)(9 1) (8 1)
(9 0) (8 0) (7 0)|(6 0)|(5 0)
(7 1) (6 1)|(4 1)|(3 1)|
(7 0) (6 0) (5 0)
(6 1) (5 1) (4 1)
(5 0) (4 0) (3 0)
(4 1) (3 1) (2 1)
(3 0) (2 0) (1 0)
(2 1)(1 1)(0 1)
(1 0) (0 0)

  A
 B C
DE FG

5) On définit d'abord des fonction de services permettant de renvoyer le prochain joueur à joueur selon, létat, le nombre d'alumettes à prélever, etc... Ces fonctions permettront un code plus lisible pour les fonctions suivantes.

La fonction action_possibles parcoure une à une toutes les actions et renvoie la liste des actions applicables sur un état donné (Si le joueur correspond et que le nombre d'allumettes est suffisant).

La fonction jouer applique simplement une action donnée à un état donné. On revérifie d'abord que l'action est bien applicable, puis on soustrait le nombre d'allumettes voulu et on change de joueur. On renvoie l'état ainsi obtenu.

Enfin la fonction successeurs_possibles prend en argument un état et, grâce aux deux fonction précédentes, renvoie la liste de tous les états qui découlent de l'état donné en appliquant chacunes des actions disponibles.

Pour les différents parcours, nous avons réalisé plusieurs fonctions : un parcours en profondeur qui renvoie l'ensemble de tous les chemins menant à l'état gagnant recherché, un second parcours en profondeur qui renvoie le premier chemin trouvé et enfin un parcours en largeur qui renvoie lui-aussi le premier parcours trouvé.


