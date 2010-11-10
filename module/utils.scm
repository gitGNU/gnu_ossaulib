
(define-module (utils)
  #:export (ensure-directory
	    system/format)
  #:export-syntax (with-working-directory))

(define-syntax with-working-directory
  (syntax-rules ()
    ((with-working-directory dir body ...)
     (let ((outer (getcwd)))
       (dynamic-wind
	 (lambda () (chdir dir))
	 (lambda () body ...)
	 (lambda () (chdir outer)))))))

(define (ensure-directory dir)
  (if (file-exists? dir)
      (or (file-is-directory? dir)
	  (error "`~a' already exists but is not a directory." dir))
      (mkdir dir)))

(define (system/format cmd . args)
  (system (apply format #f cmd args)))
