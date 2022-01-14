(setq *actions* '(
	(1 1)
	(1 2)
	(1 3)
	(0 1)
	(0 2)
	(0 3))
)

(defun prochain_joueur (etat)
	(cadr etat))

(defun nb_allumettes_total (etat)
	(car etat))

(defun joueur_action (action)
	(car action))

(defun allumettes_a_prelever (action)
	(cadr action))

(defun actions_possibles (etat)
	(let ((result NIL))
		(dolist (action *actions*)
			(if (eq (prochain_joueur etat) (joueur_action action))
				(if (>= (nb_allumettes_total etat) (allumettes_a_prelever action))
					(push action result)
				)
			)
		)
		result
	)
)

(defun etat_gagnant (x)
	(cond 
		((= x 0) (return-from etat_gagnant '(0 1)))
		((= x 1) (return-from etat_gagnant '(0 0)))
		(T NIL)
		)
	)

(defun successeurs_possibles(etat)
	(let ((result NIL) (actions (actions_possibles etat)))
		(dolist (action actions)
			(push (jouer etat action) result)
		)
		result
	)
)


(defun jouer (etat action) ;;applique l'action voulue a un état
	(if (eq (prochain_joueur etat) (joueur_action action)) ;; on vérifie que c'est au bon joueur de jouer
		(if (>= (nb_allumettes_total etat) (allumettes_a_prelever action)) ;; et que le nombre d'allumettes total est suffisant
			(if (= (prochain_joueur etat) 1)
				(list (- (nb_allumettes_total etat) (allumettes_a_prelever action)) 0)
				(list (- (nb_allumettes_total etat) (allumettes_a_prelever action)) 1)
			)
			NIL
		)
		NIL
	)
)

(defun parcours_profondeur_tous(etat parcours gagnant)
	(let ((g (etat_gagnant gagnant)))
		(push etat parcours)
		(cond 
			((EQUAL etat g) (print "le Joueur choisi a gagne")(print (reverse parcours)))
			(T
				(let ((succ (successeurs_possibles etat)))
					(dolist (xx succ)
						(parcours_profondeur_tous xx parcours gagnant)
					)
				)
			)
		)
	)
)

(defun affichage (parcours)
	(let ((precedent (pop parcours)) (dernier))
		(setq parcours (reverse parcours))
		(setq dernier (pop parcours))
		(setq parcours (reverse parcours))
		(format t "Il y a ~S allumettes, c'est au joueur ~S de jouer ~%" (car precedent) (cadr precedent))
		(dolist (x parcours)
			(dotimes (i (car precedent))
				(format t "| ")
				)
			(format t "~%")
			(dotimes (k (car precedent))
				(format t "o ")
				)
			(format t "~%")
			(format t "Il a enlevé ~S allumettes ~%" (- (car precedent) (car x)))
			(format t "~%Il y a ~S allumettes, c'est au joueur ~S de jouer ~%" (car x) (cadr x))
			(setq precedent x)
		)
		(dotimes (i (car precedent))
			(format t "| ")
			)
		(format t "~%")
		(dotimes (k (car precedent))
			(format t "o "))
		(format t "~% ~%")
		(format t "Le joueur ~S a enlevé les dernières allumettes, il a gagné" (cadr precedent))
	)
)

(defparameter sol 0)

(defun parcours_profondeur_premier(etat parcours gagnant)
	(let ((g (etat_gagnant gagnant)))
		(push etat parcours)
		(cond 
			((= sol 1) NIL)
			((EQUAL etat g) (affichage (reverse parcours)) (setq sol 1))
			(T
				(let ((succ (successeurs_possibles etat)))
					(dolist (xx succ)
						(parcours_profondeur_premier xx parcours gagnant)
					)
				)
			)
		)
	)
)

(defun end_in_file (end file) ;; vérifie si l'état solution est contenu dans l'ensemble des chemins de la file
	(dolist (xx file) ;; pour chaque chemin
		(dolist (yy xx) ;; pour chaque etat
			(if (equal yy end) (return-from end_in_file xx)) ;; on retourne le chemin contenant la solution
		)
	)
)

(defun succ_list (chemin)
	(let ((result NIL) (succs (successeurs_possibles (car chemin))))
		(dolist (xx succs)
			(push (append (list xx) chemin) result)
		)
		result
	)
)

(defun parcours_largeur (start gagnant)
	(let ((file (list (list start))) (g (etat_gagnant gagnant)))
		(loop while (null (END_IN_FILE g file)) ;;tant que la solution n'est pas dans la file
			do 
				(let ((new_file NIL)) ;; on cree une nouvelle file contenant tous les chemins decoulant des chemins de l'ancienne file
					(dolist (xx file) (setq new_file (append (SUCC_LIST xx) new_file)))
					(setq file new_file)
				)
		)
		(affichage (reverse (END_IN_FILE g file)));;on retourne le chemin trouvé
	)
)
