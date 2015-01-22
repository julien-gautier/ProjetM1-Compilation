; Evaluateur du langage intermediaire

(defun eval-li (expr env)
  ;; On test cas par cas
  (case (car expr)
	;; Si le car de l'expression est :const ou :lit, on renvoie la constante
	(:CONST (cdr expr))
	(:LIT (cdr expr))
	;; Si c'est :var, alors on renvoie la valeur a la position (cdr expr) dans la liste env
	(:VAR (aref env (cdr expr)))
	;; On retourne la variable (dans un environnement imbriqué)
	(:CVAR (cvartest env (cadr expr) (cddr expr)))
	;; On modifie la variable
	(:SET-VAR (setf (aref env (cadr expr)) (eval-li(caddr expr) env)))
	;; On modifie la variable dans l'environnement imbriqué
	(:set-cvar (setcvartest env (cadr expr) (caddr expr) (eval-li (cadddr expr) env))) ;
	(:IF (if (eval-li (cadr expr) env) (eval-li (caddr expr) env)
	       (eval-li (cadddr expr) env)))
	;; Appel d'une fonction pre-definie
	(:CALL (apply (cadr expr) (map-eval-li (cddr expr) env)))
	;; Appel d'une fonction meta-defini
	(:MCALL (let((fun (get-defun(second expr))))
		  (eval-li (third fun)
 			   (make-env-eval-li (second fun)
					     (map-eval-li (cddr expr) env)))))
	;; Appel d'une fonction locale
	(:LCALL (let((fun (cvartest env (cadr expr) (caddr expr))))
		  (eval-li (fifth fun) ; a changer en fourth si on enleve le nbr d'arg
			   (m-e-e-l-i (fourth fun) ; et ici third
				      (map-eval-li (cdddr expr) env)
				      (second fun)))))
		    
		 ; (eval-li (fifth fun) ; a changer en fourth si on enleve le nbr d'arg
 		;	   (make-env-eval-li (fourth fun) ;third
		;			     (map-eval-li (cdddr expr) env)))))
	;; Suite d'instructions li
	(:PROGN (map-eval-li-progn (cdr expr) env))
	;; Gestion du defun
	(:SET-DEFUN (set-defun (cdr (cadr expr)) (caddr expr)))
	;; Gestion des variable non connue a l'execution de lisp2li. On relance lisp2li.
	(:unknown 
	 (eval-li (lisp2li (second expr) (third expr)) env))
	;(:CLOSURE 
	;(:LCLOSURE

	; 3 cas, si symbole -> apply,  sinon -> (:closure ___ )
	;pas fini...
	(:FUNCTION ;(if (symbolp (cadr expr))
		       ;(apply (cadr expr) (cddr expr))
		  ;   
		     (if (null (get-defun (cadr expr)))
			 (if (symbolp (cadr expr))
			     (apply (cadr expr)  (map-eval-li (cddr expr) env))
			   10)
		       (get-defun (cadr expr))
		       ))
	;; Permet de creer une fermeture en capturant l'environnement courant
	(:LCLOSURE
	 `(:closure ,(make-array (length env) :initial-contents env) ,(cadr expr) ,(caddr expr) . ,(cdddr expr)))
 ;`(:closure ,env ,(cadr expr) ,(caddr expr) . ,(cdddr expr)))
	;; fonction print pour afficher
	(:PRINT (print (eval-li (cdr expr) env)))
	;; fonction quote, meme procedure que sous lisp
	(:QUOTE (quote (cdr expr)))
	;; plein de cas
	))
					;(defun get-defun <symbole>) -> valeur
;; Associe a un symbole sa definition de fonction et qui est donnee de type :lambda
(defun set-defun (symb corps)
  (setf (get symb :defun) corps))
(defun get-defun (symb)
  ( get symb :defun))

;; Construit la liste des valeurs retournée par eval-li sur chaque element de la liste
(defun map-eval-li (lexpr env)
  (if (null lexpr) ()
    (cons (eval-li (car lexpr) env) (map-eval-li (cdr lexpr) env)))
  )
;;  map-eval-li* est a map-eval-li ce que list* est a list*
(defun map-eval-li* (lexpr env)
  (if (null (cdr lexpr)) (eval-li (car lexpr) env)
    (cons (eval-li (car lexpr) env) (map-eval-li* (cdr lexpr) env)))
  )
;; Evalue en sequence les expressions de la liste et retourne la valeur de la derniere
(defun map-eval-li-progn (lexpr env)
  (if (null (cdr lexpr))
      (eval-li (car lexpr) env)
    (progn (eval-li (car lexpr) env)
	   (map-eval-li-progn (cdr lexpr) env))
    )
  )
					; Prend en parametre un entier et une liste de valeurs, crée un environnement (tableau) de la taille de l'entier (+ 1) et le remplit avec les valeurs de la liste
(defun make-env-eval-li (taille lvaleur)
    (let ((tableau (make-array (+ 1 taille))))
      (labels ((fun (listeValeur i)
		    (if (null listeValeur)
			()
		      (progn (setf (aref tableau i) (car listeValeur))
			     (fun (cdr listeValeur) (+ i 1))))))
	      (fun lvaleur 1))
    ;(print tableau)
    tableau)
  )

;; idem que make-enval-li; a l'exeption qu'on creer un tableau de taille "taille" et non "taille + 1" 
(defun make-env-eval-li-1 (taille lvaleur)
  (let(( tableau (make-array taille)))
    (labels ((fun (listeValeur i)
		  (if (null listeValeur)
		      ()
		    (progn (setf (aref tableau i) (car listeValeur))
			   (fun (cdr listeValeur) (+ i 1))))))
	    (fun lvaleur 0))
    tableau)
  )

;; make-env-eval-li, traitement similaire a celui ci exepté qu'on met l'environnement recu en parametre dans le nouvel environnement a la premiere place
;; Utilise dans le :lcall pour les environnements imbriques
(defun m-e-e-l-i (taille lvaleur env)
    (let ((tableau (make-array (+ 1 taille))))
      (setf (aref tableau 0) env)
      (labels ((fun (listeValeur i)
		    (if (null listeValeur)
			()
		      (progn (setf (aref tableau i) (car listeValeur))
			     (fun (cdr listeValeur) (+ i 1))))))
	      (fun lvaleur 1))
    ;(print tableau)
    tableau)
  )

;; Fonction utilisee pour :cvar, recherche la variable dans l'environnement imbriqué
(defun cvartest (env x y)
  (if (= x 0)
      (aref env y)
    (cvartest (aref env 0) (- x 1) y)
))

;; Fonction utilisee pour :set-cvar, recherche la variable dans l'environnement imbriqué et la remplace par la nouvelle valeur
(defun setcvartest (env x y z)
  (if (= x 0)
      (setf (aref env y) z)
    (setcvartest (aref env 0) (- x 1) y z)
))

;fonction pour tester les closures
;(eval-li '(:progn  (:print :const . cvar12) (:print :cvar 1 . 2)  (:set-cvar 1 2 (:lclosure 1 2 (:progn(:if (:call < (:var . 1) (:lit . 2))(:print :const . easy)(:progn (:lcall 2 1 (:const . 5))))))) (:print :const . go)  (:print :cvar 1 2) (:lcall 1 2 (:const . 10)) ) #(#( () (:lclosure 1 1 (:call + (:var . 1) (:const . 1))) 42 24) 100 200 ))




;(eval-li (lisp2li '(progn (defun a () (b) ) (defun b () 3 )) '()) #())
;(eval-li (lisp2li '(progn (defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 5)) '()) #() )

