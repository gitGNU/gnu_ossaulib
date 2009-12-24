
(define-module (ossau contact)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:export (fold-contacts))

(define (fold-contacts dir init proc)
  (nftw dir
	(lambda (filename statinfo flag base level)
	  (and (eq? flag 'regular)
	       (= level 1)
	       (char=? (string-ref (basename filename) 0) #\_)
	       (set! init (proc (with-input-from-file filename read-contact)
				init)))
	  #t))
  init)

(define (read-contact)
  (let loop ((contact '())
	     (field-key #f)
	     (field-value #f))
    (let ((line (read-line)))
      (if (eof-object? line)
	  (reverse! (acons field-key (reverse! field-value) contact))
	  (begin
	    ;; Format does not allow empty lines.
	    (assert (not (zero? (string-length line))))
	    ;; Check for leading space.
	    (if (char=? (string-ref line 0) #\space)
		(begin
		  ;; Start or continuation of a value.
		  (assert field-key)
		  (assert field-value)
		  (loop contact
			field-key
			(cons (substring line 1) field-value)))
		(begin
		  ;; A new field, that may or may not have an
		  ;; identifier.
		  (let ((match-data (string-match "^([^ ]+) ?([^ ]*.*)$" line)))
		    (assert match-data)
		    (loop (if field-key
			      (acons field-key (reverse! field-value) contact)
			      contact)
			  (string->symbol (match:substring match-data 1))
			  (list (match:substring match-data 2)))))))))))
