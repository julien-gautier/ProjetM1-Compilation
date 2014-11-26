;; meta evaluateur du langage intermediaire (meval LI)

(defun eval-li (expr env)
  (case (car expr)
	
	;; Si le car de l'expression est :const, on renvoie la constante
	(:CONST (cdr expr))
	;; Si c'est :var, alors on renvoie la valeur a la position (cdr expr) dans la liste env
	(:VAR (aref env (cdr expr)))
	(:CVAR (aref (aref env (cadr expr)) (cddr expr)))
	(:SET-VAR (setf (aref env (cdr expr)) (eval-li(cddr expr) env)))
	(:IF (if (eval-li (cadr expr) env) (eval-li (caddr expr) env)
	       (eval-li (cdddr expr) env)))
	(:CALL (apply (cadr expr) (map-eval-li (cddr expr) env)))
	(:MCALL (let((fun (get-defun(second expr))))
		(eval-li (first fun) ;ou third...
		(make-env-eval-li (second fun)
			(map-eval-li (cddr expr) env)))))

	(:PROGN (map-eval-li-progn (cdr expr) env))
	;(:LCLOSURE
	;(:CLOSURE
	;; for fun
	(:PRINT (print (cdr expr)))
	
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
	(setf tableau (make-array taille))
	(labels ((fun (lvaleur i)
         	(if (null lexpr)
		()
		(progn (setf (aref tableau i) (car lvaleur))
		(fun (cdr lexpr) (+ i 1))))))
  	(fun lvaleur 0))
	tableau)
	
)
  
