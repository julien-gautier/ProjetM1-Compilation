(defun lisp2li (expr env)
  ;Si expression est atome
  (if (atom expr)
	  (if (constantp expr)
	  ;Si expression est une constante
	  (cons :const expr)
	;Sinon voir si expr est une variable de l'env
	;Soit p un entier qui contient le test
	(let ((p (position expr env)))
		(if (equal p NIL)
			;Si p = nil alors erreur
			(progn (warn "variable inconnue")
			 (cons :warn expr)) 
		  ;Sinon OUI
		  (cons :var (+ p 1)))))
		  
	;Si ce n'est pas un atome
	(cond

	;Cas du MCALL
	((not (null (get-defun (car expr))))
		(list* :mcall (car expr) (map-lisp2li (cdr expr) env)))
	
	 ;Cas du IF
	 ((equal 'if (car expr))
	  (list* :if (map-lisp2li (cdr expr) env)))
	  
	;Cas du LET

	 
	
	;Cas du DEFUN
	((equal 'defun (car expr))
	 (setf (get (cadr expr) :defun) (list :lambda (length (caddr expr)) (list* :progn (map-lisp2li (cdddr expr) (make-stat-env (caddr expr) 1))))))

	
	 ;Cas du SETF
	 ((equal 'setf (car expr))
	  ;Si le 1er est un symbole
	  (if (symbolp (cadr expr)) 
	  (let ((p (position (cadr expr) env)))
		(if (equal p NIL)
		;Si p = nil alors erreur
		(warn "variable inconnue")
		  ;Sinon
		  (list :set-var (+ p 1) (lisp2li (caddr expr) env))))))

	 ;MACRO
	 ((macro-function (car expr)) (lisp2li (macroexpand-1 expr) env))

	 ;Cas du QUOTE
	 ((equal 'quote (car expr))
	  (cons :quote (cadr expr)))

	 ;Cas du PRINT
	 ((equal 'print (car expr))
	  (cons :print (lisp2li (cadr expr) env)))

	 ;Cas du PROGN
	 ((equal 'progn (car expr))
	  (cons :progn (map-lisp2li (cdr expr) env)))
	   
	 ;Cas par defaut (CALL)
	 (T (list* :call (car expr) (map-lisp2li (cdr expr) env)))
)
))

(defun map-lisp2li (expr env)
  (if (atom expr)
	  '()
	;Construit la liste des valeurs retournees par lisp2li
	;sur chaque element de expr
	(cons (lisp2li (car expr) env) (map-lisp2li (cdr expr) env))
))

;Fonction du cours
(defun get-defun (symbole)
	(get symbole :defun))
	
	
;Fonction qui construit un env local a une fonction
(defun make-stat-env (args pos)
 (if (null args)
     ()
   (if (atom args)
     (cons args pos)
   ;On construit la liste au fur et a mesure
   (cons (make-stat-env (car args) pos) (make-stat-env (cdr args) (+ 1 pos))))))
	
