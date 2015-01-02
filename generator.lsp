(defun nbInstr
	(expr)
	(cond ((eql NIL expr) 0)
		((atom expr) 0)
		((listp (car expr)) (+ (nbInstr (car expr)) (nbInstr (cdr expr))))
		((or (eql :call (car expr)) (eql :if (car expr)) (eql :progn (car expr))) (+ 1 (nbInstr (cdr expr))))
		((or (eql :const (car expr)) (eql :var (car expr))) (+ 1 (nbInstr (cdr expr))))
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
		(T (append (list (cons :CONST (car env))) (charge-env (cdr env))))
	)
)

(defun create-env
	(n)
	(if (= 0 n)
		NIL
		(append '((:CONST . NIL)) (create-env (- n 1))))
)

(defun import-env
	(n i)
	(if (> i n)
		NIL
		(append (list (cons :VAR i)) (import-env n (+ i 1))))
)

(defun compil2
	(symb expr &rest ll)
	(if (eql NIL expr)
		NIL
		(append (compil expr) (list (cons :CALL symb)) (apply #'compil2 symb (car ll) (cdr ll)))
	)
)

(defun compil
	(expr &rest ll) ; (compil <expr1> <expr2> ...) 
	(cond ((eql NIL expr) NIL); si expr est nul -> NIL 
		((atom expr) (progn (warn "ERROR : J'ai pas reçu une expression LI abruti!") NIL)) ; si expr nest pas une list, ce nest pas une instruction -> NIL
		((eql :unknown (car expr))
			(progn (warn "ERROR : Je suis pas sensé avoir un :unknown crétin!") NIL); si je reçois un :unknown, probleme dans Lisp2li
			)
		((eql :const (car expr))
			(append (list (cons :CONST (cdr expr))) (apply #'compil (car ll) (cdr ll)))
			)
		((eql :var (car expr))
			(append (list (cons :VAR (cdr expr))) (apply #'compil (car ll) (cdr ll)))
			)
		((eql :mcall (car expr))
			(append (apply #'compil (cdr expr)) (list (cons :STACK (cadr (get-defun (caar expr))))) (apply #'compil (cddr (get-defun (caar expr)))) '((:RTN)))
			)
		((eql :call (car expr))
			(cond ((eql (cadr expr) '<) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . <)) (apply #'compil (car ll) (cdr ll))))
				((eql (cadr expr) '>) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . >)) (apply #'compil (car ll) (cdr ll))))
				((eql (cadr expr) '=) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . =)) (apply #'compil (car ll) (cdr ll))))
				((eql (cadr expr) '/=) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . /=)) (apply #'compil (car ll) (cdr ll))))
				((eql (cadr expr) '<=) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . <=)) (apply #'compil (car ll) (cdr ll))))
				((eql (cadr expr) '>=) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . >=)) (apply #'compil (car ll) (cdr ll))))
				(T	(append (compil (caddr expr)) (apply #'compil2 (cadr expr) (cdddr expr))(apply #'compil (car ll) (cdr ll))))
			))
		((eql :if (car expr))
			(append (apply #'compil (list (cadr expr))) (list (cons :SKIPNIL (+ 1 (nbInstr (caddr expr))))) (apply #'compil (list (caddr expr))) (list (cons :SKIP (nbInstr (cdddr expr)))) (apply #'compil (cdddr expr)) (apply #'compil (car ll) (cdr ll)))
			)
		((eql :set-var (car expr))
			(append (apply #'compil (cddr expr)) (list (cons :SET-VAR (cadr expr))) (apply #'compil (car ll) (cdr ll)))
			)
		((eql :let (car expr))
			(append (create-env (cadr expr)) (list (cons :STACK (cadr expr))) (compil (cddr expr)) '((:RTN)) (apply #'compil (car ll) (cdr ll)))
			)
		((eql :lclosure (car expr))
			(append (import-env (cadr expr) 1) (list (cons :STACK (cadr expr))) (compil (cddr expr)) '((:RTN)) (apply #'compil (car ll) (cdr ll)))
			)
		((eql :set-fun (car expr))
			(append (list (cons :LABEL (cadr expr))) (apply #'compil (cddr expr)))
			)
		((eql :apply (car expr))
			(append (list (cons :CONST (eval-li (cdr expr) '()))) (apply #'compil (car ll) (cdr ll)))
			)
		((eql :cvar (car expr))
			NIL ; Tu vas en maxi - chier
			)
		((eql :set-cvar (car expr))
			NIL ; La  aussi
			)
		((eql :lcall (car expr))
			NIL ; TODO
			)
		((eql :progn (car expr))
			(append (apply #'compil (cdr expr) ll))
			)
		((listp (car expr))
			(apply #'compil (car expr))
			)
	)
)

(defun li2svm
	(expr env)
	(cond ((eq NIL env) (compil expr))
		((atom env) (compil expr))
		(T (progn (setq ENVIRON (make-array 10)) (setf (aref ENVIRON 0) 1) (append (charge-env env) (list (cons :STACK (nbVar env))) (compil expr) '((:RTN)))))
	)
)
