
(define-module (ossau bbdb-to)
  #:use-module (ice-9 popen)
  #:use-module (ossau contact)
  #:export (bbdb-to))

(define install-prefix (car %load-path))

(define (bbdb-to dir)
  "Read contact files from DIR and generate corresponding BBDB database."
  (let ((pipe (open-output-pipe
	       (format #f
		       "emacs --batch -q --no-site-file -l \"~a/bbdb-to.el\""
		       install-prefix))))
    (with-output-to-port pipe
      (lambda ()
	(fold-contacts dir #f (lambda (contact acc)
				(write contact)
				(newline)))))
    (close-pipe pipe)))
