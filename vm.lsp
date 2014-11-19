; CLISP : démarrer le fichier avec (load 'vm) ou (load 'vm.lsp)

(setf listvm (list)) ; créé la liste des vm

;; retourne la pile d'une vm
(defun stack
	(vm)
	(get vm 'stack))

;; retourne le pointeur de pile
(defun SP
	(vm)
	(get vm 'SP))

(defun aset
	(value tabeau elem)
	(setf (aref tabeau elem) value)
)

;; empile dans la pile de la vm
(defun push2
  	(value vm)
	(progn
		(setf (SP vm) (+ (SP vm) 1))
 		(aset value (stack vm) (SP vm))
	)
)

;; dépile la pile de la vm
(defun pop2
	(vm)
	(progn
		(setf (SP vm) (- (SP vm) 1))
		(aref (stack vm) (+ (SP vm) 1))
	)
)

;; créer une machine virtuelle
;; @param assemblercode : ((<keyword> . <etiquette>) ...)
(defun vm-make 
  	(assemblercode &key (memsize 100) (vm 'vm))	; vm === nom de la vm
  	(assert (>= memsize 10) (memsize vm) "La taille minimum requise est 10.")
  	(assert (symbolp vm) (memsize vm) "~s n'est pas un symbole." name)
  	(assert (eq (member vm listvm) NIL) (memsize vm) "La vm ~s existe déjà." vm)
  	(print 'Debut-creation-vm)
  	(setf (get vm 'stack) (make-array memsize))	; on créé la pile sous forme d'un tableau de taille memsize
	(setf (get vm 'SP) 0)
  	(print 'Pile-créée)
  	(push2 'elem vm)
	(pop2 vm)
	(print 'end)
)
