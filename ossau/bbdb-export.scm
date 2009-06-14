
(define-module (ossau bbdb-export)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (bbdb-export))

(define install-prefix (car %load-path))

(define (bbdb-export bbdb-file)
  "Read a BBDB database file and generate corresponding master format
contact files in the current working directory."
  (let ((pipe (open-output-pipe
	       (format #f
		       "emacs --batch -q --no-site-file -l \"~a/bbdb-export.el\""
		       install-prefix))))
    (write bbdb-file pipe)
    (close-pipe pipe)))
