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
	(assert (> (SP) 0) (vm) "Pile vide")
	(progn
		;(print 'POP)
		(SP--)
		(aref (stack) (SP))
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
				(':CONST (print ":CONST")
					(push2 vm (cdar assemblercode)))
				(':VAR	(print ":VAR")
					(push2 vm (aref (stack) (- (cdar assemblercode) 1))))
				(':CALL	(print ":CALL")
					(push2 vm (apply (cdar assemblercode) (pop2 vm) (pop2 vm) '())))
				(T			(print ":undefined"))
			)
			(run (cdr assemblercode) vm)
		)
	)
)

;; créer une machine virtuelle
;; @param assemblercode : ((<keyword> . <etiquette>) ...)
(defun vm-make 
  	(assemblercode &key (memsize 100) (vm 'vm))	; vm === nom de la vm
	(print 'START)
  	(assert (>= memsize 10) (memsize) "La taille minimum requise est 10.")
  	(assert (symbolp vm) (vm) "~s n'est pas un symbole." name)
  	(assert (eq (member vm listvm) NIL) (vm) "La vm ~s existe déjà." vm)
  	(print "Debut création vm")
	(setf (stacksize) (- memsize 1))
  	(setf (stack) (make-array memsize))	; on créé la pile sous forme d'un tableau de taille memsize
	(setf (SP) 0) ; stack pointer = 1 (en début de pile)
	(print "Initialisation terminée")	
	(print "Début de l'exécution...")
	(run assemblercode vm)
	'END
)
