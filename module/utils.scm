
(define-module (utils)
  #:export-syntax (with-working-directory))

(define-syntax with-working-directory
  (syntax-rules ()
    ((with-working-directory dir body ...)
     (let ((outer (getcwd)))
       (dynamic-wind
	 (lambda () (chdir dir))
	 (lambda () body ...)
	 (lambda () (chdir outer)))))))
