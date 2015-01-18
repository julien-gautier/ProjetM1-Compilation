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
	(assert (< (SP) (stacksize)) (vm) "Pile pleine")
	(prog1
		;(print 'PUSH)
 		(aset value (stack) (SP))
		(SP++)
	)
)

;; dépile la pile de la vm
(defun pop2
	(vm)
	(assert (> (SP) (FP)) (vm) "Pile vide")
	(progn
		;(print 'POP)
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

;; parcours l'ensemble des instructions assembleurs
(defun run
	(assemblercode vm)
	(if (eq assemblercode NIL)
		(print "Fin de l'exécution")
		(progn
			;(print 'READ-INST)
			(case (caar assemblercode)
				; empile le littéral
				(:CONST (print ":CONST")
					(push2 vm (cdar assemblercode)))

				; empile la n-ième variable du bloc de pile courant
				(:VAR	(print ":VAR")
					(push2 vm (aref (stack) (+ (FP) (- (cdar assemblercode) 1)))))

				; affecte la valeur dépillée à la n-ième variable du bloc de pile courant
				(:SET-VAR	(print ":SET-VAR")
					(aset (pop2 vm) (stack) (+ (FP) (- (cdar assemblercode) 1))))

				; réserve un bloc de pile de taille n
				(:STACK	(print ":STACK")
					(push2 vm (FP)) ; sauvegarde l'ancien FP (et met implicitement le SP au bon endroit)
					(setf (FP) (- (SP) (cdar assemblercode) 1))) ; place le nouveau FP

				; appelle la fonction qui dépile ses paramètres et empile son résultat
				(:CALL	(print ":CALL") ; stack(op1 op2 call) === op1 call op2
					(let ((op2 (pop2 vm)) (op1 (pop2 vm)) (fct (cdar assemblercode)))
						(if (and (symbolp fct) (eq (aref (label) fct) NIL)) ; -> ne marche pas ???
							(print fct);(push2 vm (apply fct op1 op2 '())) ; opérateur binaire uniquement d'après la convention
							(print "FONCTION DEFINIE DANS UN LABEL") ; fonction dans un label (A FAIRE)
						)
					)
				)

				; retourn d'un appel précédent
				(:RTN	(print ":RTN")
					(let ((valueToReturn (pop2 vm)) (previousFP (pop2 vm)))
						(push2 vm valueToReturn)
						(setf (FP) previousFP)))

				; saute n instructions
				(:SKIP	(print ":SKIP")
					(setf assemblercode (skip assemblercode (cdar assemblercode))))

				; saute n instructions si la valeur dépilée est NIL
				(:SKIPNIL	(print ":SKIPNIL")
					(if (eq (pop2 vm) NIL)
						(setf assemblercode (skip assemblercode (cdar assemblercode)))))

				; saute n instructions si la valeur dépilée n'est pas NIL
				(:SKIPTRUE	(print ":SKIPTRUE")
					(if (not (eq (pop2 vm) NIL))
						(setf assemblercode (skip assemblercode (cdar assemblercode)))))

				; empile le contenu du mot mémoire d'adresse n
				(:LOAD	(print ":LOAD")
					(push2 vm (aref (stack) (cdar assemblercode))))

				; affecte au mot mémoire d'adresse n la valeur dépilée (ATTENTION)
				(:STORE	(print ":STORE")
					(aset (pop2 vm) (stack) (cdar assemblercode)))

				; déclaration d'étiquette
				(:LABEL	(print ":LABEL")
					(let ((fctName (cdar assemblercode)))
					(print fctName)
					(aset (SP) (label) fctName)))

				; instruction non connue
				(T			(print ":undefined"))
			)
			(run (cdr assemblercode) vm)
		)
	)
)
 

;; @param assemblercode : ((<keyword> . <etiquette>) ...)
;; @param memsize : (optionnel) taille de la pile 
;; @param vm : (optionnel) nom de la vm qui doit être unique
(defun vm-make 
  	(assemblercode &key (memsize 100) (vm 'vm))	; vm === nom de la vm
	(print 'START)
  	(assert (>= memsize 10) (memsize) "La taille minimum requise est 10.")
  	(assert (symbolp vm) (vm) "~s n'est pas un symbole." name)
  	(assert (eq (member vm listvm) NIL) (vm) "La vm ~s existe déjà." vm)
  	(print "Debut création vm")
	(setf (stacksize) (- memsize 1))
  	(setf (stack) (make-array memsize))	; on créé la pile sous forme d'un tableau de taille memsize
	(setf (label) (make-array 20))	; 20 labels
	(setf (BP) 10)   ; base  pointer : début zone pile (ne change jamais)
	(setf (FP) (BP)) ; frame pointer : début du pile du bloc de pile courant
	(setf (SP) (BP)) ; stack pointer : pointeur sur le bloc de pile courant
	(print "Initialisation terminée")
	(print "Début de l'exécution...")
	(run assemblercode vm)
	(pop2 vm)	; sortie du code (assemblercode) donné en paramètre
	;'END
)
