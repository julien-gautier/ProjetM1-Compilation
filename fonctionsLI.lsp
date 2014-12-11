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
