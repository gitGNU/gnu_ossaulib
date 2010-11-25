
(cond-expand (guile-2
	      (eval-when (load compile)
			 (let* ((bindir (dirname (car (command-line))))
				(absdir (cond ((string=? bindir ".")
					       (getcwd))
					      ((string-match "^/" bindir)
					       bindir)
					      (else
					       (in-vicinity (getcwd) bindir)))))
			   (set! %load-path (cons (in-vicinity absdir "..")
						  %load-path)))))
	     (else
	      (let* ((bindir (dirname (car (command-line))))
		     (absdir (cond ((string=? bindir ".")
				    (getcwd))
				   ((string-match "^/" bindir)
				    bindir)
				   (else
				    (in-vicinity (getcwd) bindir)))))
		(set! %load-path (cons (in-vicinity absdir "..")
				       %load-path)))))
