
;; Compatibility definitions to add some Guile 2.0 APIs to older
;; Guiles.

(cond-expand (guile-2)

	     (guile

	      (define (current-filename)
		(let* ((script (car (command-line)))
		       (scriptdir (dirname script))
		       (absdir (cond ((string=? scriptdir ".")
				      (getcwd))
				     ((string-match "^/" scriptdir)
				      scriptdir)
				     (else
				      (in-vicinity (getcwd) scriptdir)))))
		  (in-vicinity scriptdir (basename script))))

	      (define (add-to-load-path dir)
		(set! %load-path (cons dir %load-path))))
	     
	     )
