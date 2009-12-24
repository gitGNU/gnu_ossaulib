
(define-module (ossau bbdb-from)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (bbdb-from))

(define install-prefix (car %load-path))

(define (bbdb-from bbdb-file)
  "Read a BBDB database file and generate corresponding master format
contact files in the current working directory."
  (let ((pipe (open-output-pipe
	       (format #f
		       "emacs --batch -q --no-site-file -l \"~a/bbdb-from.el\""
		       install-prefix))))
    (write bbdb-file pipe)
    (close-pipe pipe)))
