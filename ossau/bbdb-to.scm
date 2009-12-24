
(define-module (ossau bbdb-to)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:export (bbdb-to))

(define-macro (assert condition)
  `(or ,condition
       (error "Assertion failed:" ',condition)))

(define install-prefix (car %load-path))

(define (bbdb-to dir)
  "Read contact files from DIR and generate corresponding BBDB database."
  (let ((pipe (open-output-pipe
	       (format #f
		       "emacs --batch -q --no-site-file -l \"~a/bbdb-to.el\""
		       install-prefix))))
    (with-output-to-port pipe
      (lambda ()
	(nftw dir maybe-import-contact)))
    (close-pipe pipe)))

(define (maybe-import-contact filename statinfo flag base level)
  (and (eq? flag 'regular)
       (= level 1)
       (char=? (string-ref (basename filename) 0) #\_)
       (begin
	 ;;(format #t "Importing ~a\n" (basename filename))
	 (with-input-from-file filename import-contact)))
  #t)

(define (import-contact)
  (let loop ((contact '())
	     (field-key #f)
	     (field-value #f))
    (let ((line (read-line)))
      (if (eof-object? line)
	  (begin
	    ;; Send the contact to Emacs for further processing.
	    (write (reverse! (acons field-key (reverse! field-value) contact)))
	    (newline))		   
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
