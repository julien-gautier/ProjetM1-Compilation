(defun nbInstr
	(expr)
	(cond ((eql NIL expr) 0)
		((atom expr) 0)
		((listp (car expr)) (+ (nbInstr (car expr)) (nbInstr (cdr expr))))
		((or (eql ':call (car expr)) (eql ':if (car expr)) (eql ':progn (car expr))) (+ 1 (nbInstr (cdr expr))))
		((or (eql ':const (car expr)) (eql ':var (car expr))) (+ 1 (nbInstr (cdr expr))))
		(T (nbInstr (cdr expr)))
	)
)

(defun nbVar
	(env)
	(cond ((eq NIL env) 0)
		((atom env) 0)
		(T (+ 1 (nbVar (cdr env))))
	)
)

(defun charge-env
	(env)
	(cond ((eq NIL env) NIL)
		((atom env) NIL)
		(T (append (list (cons ':CONST (car env))) (charge-env (cdr env))))
	)
)

(defun get-defun
	()
)

(defun compil2
	(symb expr &rest ll)
	(if (eql NIL expr)
		NIL
		(append (compil expr)(list (cons ':CALL symb)) (apply #'compil2 symb (car ll) (cdr ll)))
	)
)

(defun compil
	(expr &rest ll) ; (compil <expr1> <expr2> ...) 
	(cond ((eql NIL expr) NIL); si expr est nul -> NIL 
		((atom expr) (progn (warn "ERROR") NIL)) ; si expr n'est pas une list, ce n'est pas une instruction -> NIL
		((eql ':const (car expr))
			(append (list (cons ':CONST (cdr expr))) (apply #'compil (car ll) (cdr ll)))
			)
		((eql ':var (car expr))
			(append (list (cons ':VAR (cdr expr))) (apply #'compil (car ll) (cdr ll)))
			)
		((eql ':mcall (car expr))
			(append (apply #'compil (cdr expr)) (list (cons ':STACK (cadr (get-defun (caar expr))))) (apply #'compil (cddr (get-defun (caar expr)))) '((RTN)))
			)
		((eql ':call (car expr))
			(cond ((or (eql (cadr expr) '+) (eql (cadr expr) '-) (eql (cadr expr) '*) (eql (cadr expr) '/))
					(append (compil (caddr expr)) (apply #'compil2 (cadr expr) (cdddr expr))(apply #'compil (car ll) (cdr ll))))
				((eql (cadr expr) '<) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . <)) (apply #'compil (car ll) (cdr ll))))
				((eql (cadr expr) '>) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . >)) (apply #'compil (car ll) (cdr ll))))
				((eql (cadr expr) '=) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . =)) (apply #'compil (car ll) (cdr ll))))
				((eql (cadr expr) '/=) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . /=)) (apply #'compil (car ll) (cdr ll))))
				((eql (cadr expr) '<=) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . <=)) (apply #'compil (car ll) (cdr ll))))
				((eql (cadr expr) '>=) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . >=)) (apply #'compil (car ll) (cdr ll))))
			))
		((eql ':if (car expr))
			(append (apply #'compil (list (cadr expr))) (list (cons ':SKIPNIL (+ 1 (nbInstr (caddr expr))))) (apply #'compil (list (caddr expr))) (list (cons ':SKIP (nbInstr (cdddr expr)))) (apply #'compil (cdddr expr)) (apply #'compil (car ll) (cdr ll)))
			)
		((eql ':set-var (car expr))
			(append (compil (cddr expr)) (list (cons ':SET-VAR (cadr expr))) (apply #'compil (car ll) (cdr ll)))
			)
		((eql ':progn (car expr))
			(append (apply #'compil (cdr expr) ll))
			)
		((listp (car expr))
			(append (apply #'compil (car expr) (cdr expr)) (apply #'compil (car ll) (cdr ll)))
			)
	)
)

(defun li2svm
	(expr env)
	(cond ((eq NIL env) (compil expr))
		((atom env) (compil expr))
		(T (append (charge-env env) (list (cons ':STACK (nbVar env))) (compil expr) '((:RTN))))
	)
)
