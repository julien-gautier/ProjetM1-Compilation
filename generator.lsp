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

(defun addvar
	(int)
	(if (> int 1)
		(addvar (/ int 10))
		int
	)
)

(defun compil
	(expr &rest ll) ; (compil <expr1> <expr2> ...)
	(cond ((eql NIL expr) NIL); si expr est nul -> NIL
		((atom expr) (progn (warn (cons "ERROR : J'ai pas recu une expression LI!" expr)) NIL)) ; si expr nest pas une list, ce nest pas une instruction -> NIL
		((eql :unknown (car expr))
			(append (compil (lisp2li (second expr) (third expr))) (apply #'compil (car ll) (cdr ll)))
		)
		((eql :const (car expr))
			(append (list (cons :CONST (cdr expr))) (apply #'compil (car ll) (cdr ll)))
		)
		((eql :var (car expr))
			(append (list (cons :VAR (cdr expr))) (apply #'compil (car ll) (cdr ll)))
		)
		((eql :mcall (car expr))
			(if (eq NIL (cddr expr))
				(append (list (cons :CALL (cadr expr))) (apply #'compil (car ll) (cdr ll)))
				(append (compil (list (cddr expr))) (list (cons :CALL (cadr expr))) (apply #'compil (car ll) (cdr ll))))
		)
		((eql :call (car expr))
			(cond ((eql (cadr expr) '<) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . <)) (apply #'compil (car ll) (cdr ll))))
				((eql (cadr expr) '>) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . >)) (apply #'compil (car ll) (cdr ll))))
				((eql (cadr expr) '=) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . =)) (apply #'compil (car ll) (cdr ll))))
				((eql (cadr expr) '/=) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . /=)) (apply #'compil (car ll) (cdr ll))))
				((eql (cadr expr) '<=) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . <=)) (apply #'compil (car ll) (cdr ll))))
				((eql (cadr expr) '>=) (append (apply #'compil (caddr expr) (cdddr expr)) '((:CALL . >=)) (apply #'compil (car ll) (cdr ll))))
				(T	(append (compil (caddr expr)) (apply #'compil2 (cadr expr) (cdddr expr)) (apply #'compil (car ll) (cdr ll))))
		))
		((eql :if (car expr))
			(append (apply #'compil (list (cadr expr))) (list (cons :SKIPNIL (+ 1 (nbVar (compil (caddr expr)))))) (compil (caddr expr)) (list (cons :SKIP (nbVar (apply #'compil (cdddr expr))))) (apply #'compil (cdddr expr)) (apply #'compil (car ll) (cdr ll)))
		)
		((eql :set-var (car expr))
			(append (apply #'compil (list (cddr expr))) (list (cons :SET-VAR (cadr expr))) (apply #'compil (car ll) (cdr ll)))
		)
		((eql :let (car expr))
			(append (create-env (cadr expr)) (list (cons :STACK (cadr expr))) (apply #'compil (cddr expr)) '((:RTN)) (apply #'compil (car ll) (cdr ll)))
		)
		((eql :lambda (car expr))
			(append (list (cons :STACK (cadr expr))) (apply #'compil (cddr expr)) '((:RTN)) (apply #'compil (car ll) (cdr ll)))
		)
		((eql :set-defun (car expr))
			(append (list (cons :LABEL (cdr (cadr expr)))) (list (cons :CONST (nbVar (compil (caddr expr))))) (list (cons :CONST (cadr (caddr expr)))) (apply #'compil (list (caddr expr)))(apply #'compil (car ll) (cdr ll)))
		)
		((eql :set-fun (car expr))
			(append (list (cons :LABEL (cadr expr))) (list (cons :CONST (nbVar (apply #'compil (cdddr expr))))) (list (cons :CONST (caddr expr))) (apply #'compil (cdddr expr))(apply #'compil (car ll) (cdr ll)))
		)
		((eql :apply (car expr))
			(append (list (cons :CONST (eval-li (cdr expr) '()))) (apply #'compil (car ll) (cdr ll)))
		)
		((eql :cvar (car expr))
			(append (list (cons :LOAD (+ (- 0 (cadr expr)) (addvar (cddr expr))))) (apply #'compil (car ll) (cdr ll)))
		)
		((eql :set-cvar (car expr))
			(append (apply #'compil (cdddr expr)) (list (cons :STORE (+ (- 0 (cadr expr)) (addvar (caddr expr))))) (apply #'compil (car ll) (cdr ll)))
		)
		((eql :lcall (car expr))
			(append (apply #'compil (cdddr expr)) (list (cons :LOAD (+ (- 0 (cadr expr)) (addvar (cddr expr))))) (apply #'compil (car ll) (cdr ll)))
		)
		((eql :progn (car expr))
			(append (apply #'compil (list (cdr expr)) ll))
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

; EXEMPLE:
;
;(li2svm '(:set-defun (:const . fib) (:lambda 1 (:if (:call < (:var . 1) (:const . 2)) (:call + (:mcall FIB (:call - (:var . 1) (:const . 1))) (:mcall FIB (:call - (:var . 1) (:const . 2))))))) ())
;
;
