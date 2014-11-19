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

     ;Cas du IF
     ((equal 'if (car expr))
      (cons :if (map-lisp2li (cdr expr) env)))

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

     ;Cas du QUOTE
     ((equal 'quote (car expr))
      (cons :const env))

     ;Cas du PROGN
     ((equal 'progn (car expr))
      (cons :progn (map-lisp2li (cdr expr) env)))
	   
     ;Cas par defaut
     ((list :call (car expr)) (map-lisp2li (cdr expr) env))
      
)
))

(defun map-lisp2li (expr env)
  (if (atom expr)
      '()
    ;Construit la liste des valeurs retournees par lisp2li
    ;sur chaque element de expr
    (cons (lisp2li (car expr) env) (map-lisp2li (cdr expr) env))
))
