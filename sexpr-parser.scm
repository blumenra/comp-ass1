(load "pc.scm")

(define <sexpr>
  ;; fill in the s-expression parser details here
  )

(define <Boolean>
    (new
    	(*parser (word-ci "#t"))
    	(*parser (word-ci "#f"))
    	(*disj 2)
    	(*pack (lambda (str)
    		(let ((l (char-downcase (cadr str))))
 				(cond
 					((char=? l #\t) #t)
 					((char=? l #\f) #f)
 					(else (display "The value is no valid!"))))))

    	done))

