; CLISP : démarrer le fichier avec (load 'vm) ou (load 'vm.lsp)

(setf listvm (list)) ; créé la liste des vm

;; retourne la pile d'une vm
(defmacro stack
	()
	'(get vm 'stack))

;; retourne la taille de la pile d'une vm
(defmacro stacksize
	()
	'(get vm 'stacksize))


;; retourne la liste des labels enregistrés
(defmacro label
	()
	'(get vm 'label))

;; retourne le nombre max de labels enregistrés
(defmacro maxlabel
	()
	'(get vm 'maxlabel))


;; retourne le début de la zone pile (avant BP => zone mémoire)
(defmacro BP
	()
	'(get vm 'BP))

;; retourne le début du bloc de pile courant
(defmacro FP
	()
	'(get vm 'FP))

;; retourne le pointeur de pile
(defmacro SP
	()
	'(get vm 'SP))

;; incrémente le SP
(defmacro SP++
	()
	'(setf (get vm 'SP) (+ (get vm 'SP) 1)))

;; décrémente le SP
(defmacro SP--
	()
	'(setf (get vm 'SP) (- (get vm 'SP) 1)))

;; modifier un élément d'un tableau
(defun aset
	(value tabeau elem)
	(setf (aref tabeau elem) value)
)

;; empile dans la pile de la vm
(defun push2
  	(vm value)
	;(assert (< (SP) (stacksize)) (vm) "Pile pleine")
	(progn
 		(SP++)
 		(if (> (SP) (stacksize))
 			(progn
 				(setf (stacksize) (+ (SP) 100))
 				(setf (stack) (adjust-array (stack) (+ (SP) 101)))
 			)
 		)
 		(aset value (stack) (- (SP) 1))		
	)
)

;; dépile la pile de la vm
(defun pop2
	(vm)
	(assert (> (SP) (FP)) (vm) "Pile vide")
	(progn
		(SP--)
		(aref (stack) (SP))
	)
)

;; saute n instructions
(defun skip
	(assemblercode n)
	(if (eq n 0)
		assemblercode
		(skip (cdr assemblercode) (- n 1))
	)
)

;; retourne une liste de n instructions
(defun getInstr
	(assemblercode n &key (instr '()))
	(if (eq n 0)
		instr
		(getInstr (cdr assemblercode) (- n 1) :instr (append instr (list (car assemblercode))))
	)
)

;; retourne l'identifiant d'une fonction à partir de son nom
(defun getIdFunction
	(vm functionName &key (i 0))
	(if (or (>= i (maxlabel)) (< i 0))
		NIL
		(if (or (and (eq functionName NIL) (eq (aref (label) i) NIL))
				(and (not (eq (aref (label) i) NIL)) (eq functionName (aref (aref (label) i) 0))))
			i
			(getIdFunction vm functionName :i (+ i 1))
		)
	)
)

;; sauvegarde une nouvelle fonction
(defun addNewFunction
	(vm fctName nbArg nbInstr code)
	(let ((newIdFunction (getIdFunction vm NIL)))
		(assert (not (eq newIdFunction NIL)) (idFunction) "WARNING : IMPOSSIBLE TO DEFINED A NEW FUNCTION !")
		(aset (make-array 3) (label) newIdFunction)   ; 0 nom, 1 nbArg, 2 code
		(aset fctName (aref (label) newIdFunction) 0) ; 0 nom
		(aset nbArg   (aref (label) newIdFunction) 1) ; 1 nbArg
		(aset code	  (aref (label) newIdFunction) 2) ; 2 code
	)
)


;; parcours l'ensemble des instructions assembleurs
(defun run
	(assemblercode vm &key (fctName NIL) (i 0) (show NIL))
	(if (eq assemblercode NIL)
			(if show
				(if (eq fctName NIL)
					(progn (format t "→")
							(loop for x from 0 to i do
								(format t "  ")
							)
							(format t "Le programme retourne ~a~%" (aref (stack) (- (SP) 1)))
					)
					(progn (format t "→")
							(loop for x from 0 to i do
								(format t "  ")
							)
							(format t "La fonction ~a retourne ~a~%" fctName (aref (stack) (- (SP) 1)))
					)
				)
			)
		(progn
			;(print 'READ-INST)
			(case (caar assemblercode)
				; empile le littéral
				(:CONST ;(format t ":CONST~%")
					(push2 vm (cdar assemblercode)))

				; empile la n-ième variable du bloc de pile courant
				(:VAR	;(format t ":VAR~%")
					(push2 vm (aref (stack) (+ (FP) (- (cdar assemblercode) 1)))))

				; affecte la valeur dépillée à la n-ième variable du bloc de pile courant
				(:SET-VAR	;(format t ":SET-VAR~%")
					(aset (pop2 vm) (stack) (+ (FP) (- (cdar assemblercode) 1))))

				; réserve un bloc de pile de taille n
				(:STACK	;(format t ":STACK~%")
					(push2 vm (FP)) ; sauvegarde l'ancien FP (et met implicitement le SP au bon endroit)
					(setf (FP) (- (SP) (cdar assemblercode) 1))) ; place le nouveau FP

				; appelle la fonction qui dépile ses paramètres et empile son résultat
				(:CALL ; stack(op1 op2 call) === op1 call op2
					(if show
						(progn 
							(format t "→")
							(loop for x from 0 to i do
								(format t "  ")
							)
							(format t ":CALL")
						)
					)
					(let ((fct (cdar assemblercode)))
						(if (or (eq fct '+) (eq fct '-) (eq fct '/) (eq fct '*) (eq fct '<) (eq fct '>) (eq fct '=) (eq fct '/=) (eq fct '<=) (eq fct '>=))
							(let ((op2 (pop2 vm)) (op1 (pop2 vm)))
								(if show (format t " (~a ~a ~a)" fct op1 op2))
								(if (and (numberp op1) (numberp op2)) ; on teste si les 2 arguments sont bien des nombres. Si ce n'est pas le cas, on renvoie -1 ou NIL
									(push2 vm (apply fct op1 op2 '())) ; opérateur binaire uniquement d'après la convention
									(progn
										(if show (format t " → WARNING : ONE ARG IS NOT A NUMBER !"))
										(if (or (eq fct '+) (eq fct '-) (eq fct '/) (eq fct '*))
											(push2 vm -1)	; + - * /
											(push2 vm NIL)) ; = /= < > <= >=
									)
								)
								(if show (format t "~%"))
							)
							(let ((idFct (getIdFunction vm fct)))
								(if (eq idFct NIL)
									(if show (format t " → WARNING : UNKNOW FUNCTION !~%"))
									; ajouter un stack avant l'appel de la fonction (sauf si elle ne prend pas d'arguement) (c'est li2svm qui se charge de le faire)
									(progn
										(if show 
											(progn
												(format t " (~a" fct)
												(if (> (aref (aref (label) idFct) 1) 0)
													(loop for x from (- (SP) (aref (aref (label) idFct) 1)) to (- (SP) 1) do
														(format t " ~a" (aref (stack) x))
													)
												)
												(format t ")~%")
											)
										)					
										(run (aref (aref (label) idFct) 2) vm :fctName fct :i (+ i 1) :show show) ; on suppose aussi que li2svm fait un return à la fin
									)
								)
							)
						)
					)
				)

				; retourn d'un appel précédent
				(:RTN	;(format t ":RTN~%")
					(let ((valueToReturn (pop2 vm)) (previousFP (pop2 vm)))
						(setf (SP) (FP))
						(push2 vm valueToReturn)
						(setf (FP) previousFP)))

				; saute n instructions
				(:SKIP	;(format t ":SKIP~%")
					(setf assemblercode (skip assemblercode (cdar assemblercode))))

				; saute n instructions si la valeur dépilée est NIL
				(:SKIPNIL	;(format t ":SKIPNIL~%")
					(if (eq (pop2 vm) NIL)
						(setf assemblercode (skip assemblercode (cdar assemblercode)))))

				; saute n instructions si la valeur dépilée n'est pas NIL
				(:SKIPTRUE	;(format t ":SKIPTRUE~%")
					(if (not (eq (pop2 vm) NIL))
						(setf assemblercode (skip assemblercode (cdar assemblercode)))))

				; empile le contenu du mot mémoire d'adresse n
				(:LOAD	;(format t ":LOAD~%")
					(push2 vm (aref (stack) (cdar assemblercode))))

				; affecte au mot mémoire d'adresse n la valeur dépilée (ATTENTION)
				(:STORE	;(format t ":STORE~%")
					(aset (pop2 vm) (stack) (cdar assemblercode)))

				; déclaration d'étiquette
				(:LABEL	;(format t ":LABEL~%")
					(let ((fctName (cdar assemblercode)) (nbInstr (cdadr assemblercode)) (nbArg (cdaddr assemblercode)))
						(addNewFunction vm fctName nbArg nbInstr (getInstr (cdddr assemblercode) nbInstr))
						(setf assemblercode (skip assemblercode (+ nbInstr 2)))))

				; instruction non connue
				(T		(format t ":undefined~%"))
			)
			(run (cdr assemblercode) vm :fctName fctName :i i :show show)
		)
	)
)


;; @param assemblercode : ((<keyword> . <etiquette>) ...)
;; @param memsize : (optionnel) taille de la pile 
;; @param vm : (optionnel) nom de la vm qui doit être unique
(defun vm-make
  	(assemblercode &key (minmemsize 100) (vm 'vm) (show NIL))	; vm === nom de la vm
  	(assert (symbolp vm) (vm) "~s n'est pas un symbole." name)
  	(assert (>= minmemsize 1) (minmemsize) "La taille minimum requise est 1.")
  	(assert (eq (member vm listvm) NIL) (vm) "La vm ~s existe déjà." vm)
  	(if show (format t "→ Initialisation de la vm~%"))
	(setf (stacksize) (+ minmemsize 9))
  	(setf (stack) (make-array (+ minmemsize 10))) ; on créé la pile sous forme d'un tableau de taille memsize
	(setf (maxlabel) 20)					; maximum 20 labels
  	(setf (label) (make-array (maxlabel)))	; on créé la liste des noms des fonctions enregistrées
  	(setf (label) (make-array (maxlabel)))	; on créé la liste des noms des fonctions enregistrées
	(setf (BP) 10)   ; base  pointer : début zone pile (ne change jamais)
	(setf (FP) (BP)) ; frame pointer : début du pile du bloc de pile courant
	(setf (SP) (BP)) ; stack pointer : pointeur sur le bloc de pile courant
	(if show (format t "→ Initialisation terminée~%"))
	(if show (format t "→ Début de l'exécution du programme~%"))
	(run assemblercode vm :show show)
	(if (> (SP) (FP))
		(pop2 vm)	; sortie du code (assemblercode) donné en paramètre
		T 			; sortie de code (rien dans la pile)
	)
)


; (vm-make (li2svm (lisp2li '(progn (defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 1)) '()) '()))


; (vm-make '((:CONST . T) (:LABEL . :add) (:CONST . 4) (:CONST . 2) (:VAR . 1) (:VAR . 2) (:CALL . +) (:RTN) (:CONST . 11) (:CONST . 24) (:STACK . 2) (:CALL . :add) (:CONST . 65) (:STACK . 2) (:CALL . :add)))


; (vm-make '((:LABEL . FIB) (:CONST . 17) (:CONST . 1) (:STACK . 1) (:VAR . 1) (:CONST . 2) (:CALL . <) (:SKIPNIL . 2) (:VAR . 1) (:SKIP . 9) (:VAR . 1) (:CONST . 1) (:CALL . -) (:CALL . FIB) (:VAR . 1) (:CONST . 2) (:CALL . -) (:CALL . FIB) (:CALL . +) (:RTN) (:CONST . 4) (:CALL . FIB)))
