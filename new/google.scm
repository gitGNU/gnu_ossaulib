
(define google-field-mapping
  '((first-names ("Given Name" "Additional Name")
		 ,(lambda (given additional)
		    (if (zero? (string-length additional))
			given
			(string-append given " " additional))))
    (last-name . "Family Name")
))
