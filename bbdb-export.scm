
(use-modules (ice-9 popen)
	     (ice-9 rdelim))

(define (bbdb-export bbdb-file base-dir)
  "Reads BBDB database file BBDB-FILE and writes corresponding contact
files in the master format to a timestamped directory under BASE-DIR."
  (let* ((timestamp (strftime "%Y%m%d-%H%M" (localtime (current-time))))
	 (outdir (in-vicinity base-dir timestamp))
	 (pwd (getcwd)))
    (false-if-exception (mkdir outdir))
    (chdir outdir)
    (with-input-from-file bbdb-file
      (lambda ()
	(let ((pipe (open-output-pipe
		     (format #f
			     "emacs --batch -q --no-site-file -l \"~a/bbdb-export.el\""
			     pwd))))
	  (write bbdb-file pipe)
	  (close-pipe pipe)
	  #f)))))

(apply bbdb-export (cdr (command-line)))
