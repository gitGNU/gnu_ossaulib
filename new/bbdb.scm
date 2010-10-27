
(define-module (contacts bbdb)
  #:use-module (contacts common)
  #:use-module (language elisp parser)
  #:use-module (oop goops)
  #:export (<bbdb>))

(define-class <bbdb> (<contacts>))

;; Read a BBDB file.
(define-method (read (db <bbdb>) bbdb-file)

  (define (open-bbdb-file)
    (catch #t
      (lambda ()
	;; First try opening the file without the binary flag.  This
	;; allows Guile to scan for a coding comment, and set the port
	;; coding accordingly - but it will throw an error if there's
	;; a comment with a coding that Guile doesn't recognise,
	;; notably `utf-8-emacs'.
	(open-file bbdb-file "r"))
      (lambda _
	;; OK, that didn't work; so open in binary mode instead, scan
	;; for an encoding, and see if it's something that we can
	;; handle by mapping to a coding that Guile supports.
	(let* ((port (open-file bbdb-file "rb"))
	       (coding (string->symbol
			(string-downcase (file-encoding port)))))
	  (set-port-encoding! port
			      (case coding
				((utf-8-emacs emacs-internal)
				 "utf-8")
				(else
				 (error "Unhandled coding: " coding))))
	  port))))

  (define (convert-nil x)
    ;; The current elisp reader doesn't convert the symbol 'nil to the
    ;; nil value #nil.  (Instead, (@ (language elisp compile-tree-il)
    ;; compile-symbol) does this when compiling elisp code.)  So we do
    ;; the conversion by hand here.
    (cond ((eq? x 'nil) #nil)
	  ((pair? x)
	   (set-car! x (convert-nil (car x)))
	   (set-cdr! x (convert-nil (cdr x)))
	   x)
	  ((vector? x)
	   (list->vector (map convert-nil (vector->list x))))
	  (else x)))
  
  (define vector-data
    ;; Read BBDB data in its native vector format.
    (let ((port (open-bbdb-file)))
      (let loop ((data '()) (x (convert-nil (read-elisp port))))
	(if (eq? x '*eoi*)
	    (begin
	      (close-port port)
	      (reverse! data))
	    (loop (cons x data) (convert-nil (read-elisp port)))))))
  
  ;; Map the native vector format to an alist.
  (set! (records db)
	(map (lambda (v-record)
	       `((X-BBDB-FIRST-NAMES . ,(vector-ref v-record 0))
		 (X-BBDB-LAST-NAME   . ,(vector-ref v-record 1))
		 (X-BBDB-AKAS        . ,(vector-ref v-record 2))
		 (X-BBDB-FIELD-3     . ,(vector-ref v-record 3))
		 (X-BBDB-PHONES      . ,(map vector->list (vector-ref v-record 4)))
		 (X-BBDB-ADDRESSES   . ,(map vector->list (vector-ref v-record 5)))
		 (X-BBDB-EMAILS      . ,(vector-ref v-record 6))
		 (X-BBDB-NOTES       . ,(vector-ref v-record 7))
		 (X-BBDB-FIELD-8     . ,(vector-ref v-record 8))))
	     vector-data)))

;; (read (make <bbdb>) "/home/neil/BBDB")

;; Given a BBDB record, compute standard contact fields from it.
(define-method (compute-standard-fields (db <bbdb>) record)
  (map-named-fields record
		    '((FIRST-NAMES . X-BBDB-FIRST-NAMES)
		      (LAST-NAME   . X-BBDB-LAST-NAME)
		      (PHONE       . X-BBDB-PHONES))))

;; Given a set of standard contact fields, compute corresponding
;; native BBDB fields.
(define-method (compute-native-fields (db <bbdb>) record)
  (map-named-fields record
		    '((X-BBDB-FIRST-NAMES . FIRST-NAMES)
		      (X-BBDB-LAST-NAME   . LAST-NAME)
		      (X-BBDB-PHONES      . PHONE))))

