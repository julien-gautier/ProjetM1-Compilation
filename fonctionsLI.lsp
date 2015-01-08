;;quelques fonctions en LI

;, FIBO simple

(:call set-defun (:const . fibo)
	(:const :lambda 1 
		(:if (:call < (:var . 1) (:const . 3))
			(:const . 1) .
			(:call +
				(:mcall fibo (:call - (:var . 1) (:const . 1)))
				(:mcall fibo (:call - (:var . 1) (:const . 2)))
			)
		)
	)
)



;; FIBO compliqué

(:closure () 1 3
	(:progn (:set-var 2
		(:lclosure 1 1
			(:if (:call < (:var . 1) (:lit . 2))
				(:lit . 1) .
				(:call +
					(:lcall 1 3 (:call - (:var . 1) (:lit . 1)))
					(:lcall 1 3 (:call - (:var . 1) (:lit . 2)))
					)
				)
		)
		)
		(:set-var 3
		(:lclosure 1 1
			(:if (:call < (:var . 1) (:lit . 2))
				(:lit . 1) .
				(:call +
					(:lcall 1 2 (:call - (:var . 1) (:lit . 1)))
					(:lcall 1 2 (:call - (:var . 1) (:lit . 2)))
				)
			)
		)
		)
		(:lcall 0 2 (:var . 1))
	)
)

;; a revoir
(:closure () 1 3
	(:progn (:set-var 2
		(:lclosure 1 1
			(:if (:call < (:var . 1) (:lit . 2))
				(:lit . 1) .
				(:call +
					(:lcall 1 3 (:call - (:var . 1) (:lit . 1)))
					(:lcall 1 3 (:call - (:var . 1) (:lit . 2)))
					)
				)
		)
		)
		(:set-var 3
		(:lclosure 1 1
			(:if (:call < (:var . 1) (:lit . 2))
				(:lit . 1) .
				(:call +
					(:lcall 1 2 (:call - (:var . 1) (:lit . 1)))
					(:lcall 1 2 (:call - (:var . 1) (:lit . 2)))
				)
			)
		)
		)
		(:lcall 0 2 (:var . 1))
	)
)


;; fonction terminale length en LI
(:call set-defun (:const . length_ct)
	(:const :lambda 2
		(:IF (:call null (:VAR . 1))
			(:VAR . 2)
			(:mcall length_ct (:call cdr (:VAR . 1))
					(:call + (:VAR . 2) (:CONST . 1))))
	)
)
