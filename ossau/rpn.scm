
(define-module (ossau rpn)
  #:use-module (ice-9 and-let-star)
  #:export-syntax (rpn-lambda test-rpn-lambda))

(define-macro (rpn-lambda hsilop . formals)
  (let ((x (with-input-from-string hsilop
	     (lambda ()
	       (let loop ((stack '()) (next (read)))
		 (cond ((eof-object? next)
			(car stack))
		       ((memq next '(exch inv neg sgn dup pop == != ^ &))
			(case next
			  ((exch)
			   (loop (cons (cadr stack) (cons (car stack) (cddr stack)))
				 (read)))
			  ((inv)
			   (loop (cons (list '/ (car stack)) (cdr stack))
				 (read)))
			  ((neg)
			   (loop (cons (list '- (car stack)) (cdr stack))
				 (read)))
			  ((sgn)
			   (loop (cons `((lambda (x)
					   (cond ((> x 0) 1)
						 ((< x 0) -1)
						 (else 0)))
					 ,(car stack))
				       (cdr stack))
				 (read)))
			  ((dup)
			   (loop (cons (car stack) stack)
				 (read)))
			  ((pop)
			   (loop (cdr stack)
				 (read)))
			  ((==)
			   (loop stack '=))
			  ((!=)
			   (loop (cons `(not (= ,(car stack) ,(cadr stack)))
				       (cddr stack))
				 (read)))
			  ((^)
			   (loop stack 'expt))
			  ((&)
			   (loop stack 'inv))))
		       ((or (not (symbol? next))
			    (memq next formals))
			(loop (cons next stack) (read)))
		       ((and-let* ((proc (false-if-exception (primitive-eval next)))
				   (arity (and (procedure? proc)
					       (procedure-property proc 'arity))))
			  (= (car arity) 1))
			(loop (cons (list next (car stack))
				    (cdr stack))
			      (read)))
		       (else
			(loop (cons (list next (cadr stack) (car stack))
				    (cddr stack))
			      (read)))))))))
    `(lambda ,formals ,x)))

(define (test-rpn-lambda)
  (for-each (lambda (calculated expected)
	      (or (equal? calculated expected)
		  (error "rpn-lambda test failed")))
	    (list ((rpn-lambda "1 2 +" x) 4)
		  ((rpn-lambda "x x *" x) 3)
		  ((rpn-lambda "x 4 -" x) 3)
		  ((rpn-lambda "x 4 exch -" x) 3)
		  ((rpn-lambda "x neg" x) 3)
		  ((rpn-lambda "x inv" x) 4)
		  ((rpn-lambda "x 4 - sgn" x) 3)
		  ((rpn-lambda "x 4 - sgn dup *" x) 3)
		  ((rpn-lambda "x y 3 4 5 pop pop * +" x y) 10 20)
		  ((rpn-lambda "x y ==" x y) 10 20)
		  ((rpn-lambda "x y !=" x y) 10 20)
		  ((rpn-lambda "x y ^" x y) 10 3)
		  ((rpn-lambda "x & 2 +" x) 4)
		  ((rpn-lambda "x cos" x) 0)
		  )
	    (list 3
		  9
		  -1
		  1
		  -3
		  0.25
		  -1
		  1
		  70
		  #f
		  #t
		  1000
		  2.25
		  1.0)))
