; meta evaluateur du langage intermediaire (meval LI)
; inutile, a enlever
; function a revoir
(defun test () 5)
(defun eval-li (expr env)
  ;;(print 'expression)
  ;;(print expr)
  ;;(print 'envir)
  ;;(print env)
  (case (car expr)
	;; Si le car de l'expression est :const, on renvoie la constante
	(:CONST (cdr expr))
	(:LIT (cdr expr))
	;; Si c'est :var, alors on renvoie la valeur a la position (cdr expr) dans la liste env
	(:VAR (aref env (cdr expr)))
	;(:CVAR (aref (aref env (cadr expr)) (cddr expr)))
	(:CVAR (cvartest env (cadr expr) (cddr expr)))
	(:SET-VAR (progn (print 'thelastone)(print 'final) (print (eval-li(caddr expr) env))(setf (aref env (cadr expr)) (eval-li(caddr expr) env))) (print env)) ;cddr
;	(:SET-VAR (setf (aref env (cadr expr)) (eval-li(cddr expr) env))) ;cddr
; faut evaluer le cadddr expr... et faire qu'une seule evaluation ans le lcall..
;	(:SET-CVAR (setf (aref (aref env (cadr expr)) (caddr expr)) (cadddr expr)))
	(:set-cvar  (progn (print 'thelastonppppe)(print 'envir) (print env)(print 'final)(print (eval-li (cadddr expr) env))(setcvartest env (cadr expr) (caddr expr) (eval-li (cadddr expr) env)) (print env))) ; ça coince ici
	;(:SET-CVAR (setf (cvartest env (cadr expr) (caddr expr)) (cadddr expr)))
	(:IF (if (eval-li (cadr expr) env) (eval-li (caddr expr) env)
	       (eval-li (cadddr expr) env)))
	(:CALL (apply (cadr expr) (map-eval-li (cddr expr) env)))
	(:MCALL (let((fun (get-defun(second expr))))
		  (eval-li (third fun)
 			   (make-env-eval-li (second fun)
					     (map-eval-li (cddr expr) env)))))
;	(:LCALL (let((fun (eval-li (aref (aref env (cadr expr)) (caddr expr)) env)))
	(:LCALL (let((fun (cvartest env (cadr expr) (caddr expr))))
		  (print 'gogogo)
		   (print (cvartest env (cadr expr) (caddr expr))) ;third
		;			     (map-eval-li (cdddr expr) env)))))
		  (eval-li (fifth fun) ; a changer en fourth si on enleve le nbr d'arg
			   (m-e-e-l-i (fourth fun) ;third
				      (map-eval-li (cdddr expr) env)
				      (second fun)))))
		    
		 ; (eval-li (fifth fun) ; a changer en fourth si on enleve le nbr d'arg
 		;	   (make-env-eval-li (fourth fun) ;third
		;			     (map-eval-li (cdddr expr) env)))))

	(:PROGN (map-eval-li-progn (cdr expr) env))
	(:SET-DEFUN (set-defun (cdr (cadr expr)) (caddr expr)))
	(:unknown 
	 (eval-li (lisp2li (second expr) (third expr)) env))
	;(:CLOSURE 
	;(:LCLOSURE

	; 3 cas, si symbole -> apply,  sinon -> (:closure ___ )
	;pas fini... pas compris
	(:FUNCTION ;(if (symbolp (cadr expr))
		       ;(apply (cadr expr) (cddr expr))
		  ;     2
		     (if (null (get-defun (cadr expr)))
			 (if (symbolp (cadr expr))
			     (apply (cadr expr)  (map-eval-li (cddr expr) env))
			   10)
		       (get-defun (cadr expr))
		       ))
;		     ))
	
	(:LCLOSURE
 `(:closure ,(make-array (length env) :initial-contents env) ,(cadr expr) ,(caddr expr) . ,(cdddr expr)))
 ;`(:closure ,env ,(cadr expr) ,(caddr expr) . ,(cdddr expr)))
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

(defun cvartest (env x y)
  (if (= x 0)
      (aref env y)
    (cvartest (aref env 0) (- x 1) y)
))

(defun setcvartest (env x y z)
  (if (= x 0)
      (setf (aref env y) z)
    (setcvartest (aref env 0) (- x 1) y z)
))

;fonction pour tester les closures
;(eval-li '(:progn  (:print :const . cvar12) (:print :cvar 1 . 2)  (:set-cvar 1 2 (:lclosure 1 2 (:progn(:if (:call < (:var . 1) (:lit . 2))(:print :const . easy)(:progn (:lcall 2 1 (:const . 5))))))) (:print :const . go)  (:print :cvar 1 2) (:lcall 1 2 (:const . 10)) ) #(#( () (:lclosure 1 1 (:call + (:var . 1) (:const . 1))) 42 24) 100 200 ))
 
 ;test unknown
 ;(eval-li (lisp2li '(progn (defun a (x) (b x) ) (defun b (x) (+ 3 x) ) (a 8)) '()) #())

;fibo
;(eval-li (lisp2li '(progn (defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 5)) '()) #() )

