
(define-module (ossau csv)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 string-fun)
  #:export (read-csv))

(define (read-line-16)
  (let ((s (read-line)))
    (or (eof-object? s)
	(read-char))
    s))

(define (read-csv file-name)
  (with-input-from-file file-name
    (lambda ()
      (set-port-encoding! (current-input-port) "UTF16LE")
      (let* ((header-line (read-line-16))
	     (headers (separate-fields-discarding-char #\, header-line list))
	     (nheaders (length headers)))
	(write (cons 'header-line header-line))
	(let loop ((data '())
		   (entry '())
		   (headers-to-read headers)
		   (line (read-line-16)))
	  (write (cons 'line line))
	  (newline)
	  (cond ((eof-object? line)
		 (reverse! data))
		((null? headers-to-read)
		 (display entry)
		 (newline)
		 (loop (cons (reverse! entry) data)
		       '()
		       headers
		       (read-line-16)))
		((char=? (string-ref line 0) #\")
		 (receive (value rest)
			  (with-input-from-string line
			    (lambda ()
			      (let* ((value (false-if-exception (read)))
				     (rest (and value
						(not (null?
						      (cdr headers-to-read)))
						(begin
						  (read-char)
						  (read-line-16)))))
				(write (cons 'value value)) (newline)
				(write (cons 'rest rest)) (newline)
				(values value rest))))
			  (if value
			      (loop data
				    (acons (car headers-to-read)
					   value
					   entry)
				    (cdr headers-to-read)
				    rest)
			      (loop data
				    entry
				    headers-to-read
				    (string-append line "\n" (read-line-16))))))
		(else
		 (split-discarding-char #\, line
					(lambda (value rest)
					  (loop data
						(acons (car headers-to-read)
						       value
						       entry)
						(cdr headers-to-read)
						rest))))))))))
