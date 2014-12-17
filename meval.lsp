; meta evaluateur du langage intermediaire (meval LI)
(defun eval-li (expr env)
  (case (car expr)
	;; Si le car de l'expression est :const, on renvoie la constante
	(:CONST (cdr expr))
	(:LIT (cdr expr))
	;; Si c'est :var, alors on renvoie la valeur a la position (cdr expr) dans la liste env
	(:VAR (aref env (cdr expr)))
	(:CVAR (aref (aref env (cadr expr)) (cddr expr)))
	(:SET-VAR (setf (aref env (cadr expr)) (eval-li(caddr expr) env))) ;cddr
;	(:SET-VAR (setf (aref env (cadr expr)) (eval-li(cddr expr) env))) ;cddr
	(:IF (if (eval-li (cadr expr) env) (eval-li (caddr expr) env)
	       (eval-li (cdddr expr) env)))
	(:CALL (apply (cadr expr) (map-eval-li (cddr expr) env)))
	(:MCALL (let((fun (get-defun(second expr))))
		  (eval-li (third fun)
 			   (make-env-eval-li (second fun)
					     (map-eval-li (cddr expr) env)))))
	(:PROGN (map-eval-li-progn (cdr expr) env))
	;(:CLOSURE 
	;(:LCLOSURE

	; 3 cas, si symbole -> apply,  sinon -> (:closure ___ )
	;pas fini... pas compris
;	(:FUNCTION (if (symbp (cadr expr))
;		       (apply (cadr expr) (cddr expr))
;		     ))
	
	;; for fun
	(:PRINT (print (eval-li (cdr expr) env)))
	(:QUOTE (quote (cdr expr)))
	;; plein de cas
	))
					;(defun get-defun <symbole>) -> valeur
(defun get-defun (symb)
  ( get symb :defun))
(defun set-defun (symb corps)
  (setf (get symb :defun) corps))
(defun map-eval-li (lexpr env)
  (if (null lexpr) ()
    (cons (eval-li (car lexpr) env) (map-eval-li (cdr lexpr) env)))
  )
(defun map-eval-li* (lexpr env)
  (if (null (cdr lexpr)) (eval-li (car lexpr) env)
    (cons (eval-li (car lexpr) env) (map-eval-li* (cdr lexpr) env)))
  )
(defun map-eval-li-progn (lexpr env)
  (if (null (cdr lexpr))
      (eval-li (car lexpr) env)
    (progn (eval-li (car lexpr) env)
	   (map-eval-li-progn (cdr lexpr) env))
    )
  )
					; Prend en parametre un entier et une liste de VALEURS, crée un environnement (tableau) de la taille de l'entier et le remplit avec les valeurs de la liste
(defun make-env-eval-li (taille lvaleur)
  (progn
    (setf tableau (make-array (+ 1 taille)))
    (labels ((fun (listeValeur i)
		  (if (null listeValeur)
		      ()
		    (progn (setf (aref tableau i) (car listeValeur))
			   (fun (cdr listeValeur) (+ i 1))))))
	    (fun lvaleur 1))
    ;(print tableau)
    tableau)
  )

;; on fait pas le +1
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
