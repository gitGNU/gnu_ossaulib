
(define-module (contacts common)
  #:use-module (oop goops)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:export (fold-contacts
	    <contacts>
	    records
	    map-named-fields
	    native-read
	    native-write
	    compute-standard-fields
	    compute-native-fields)
  #:replace (import))

(define-generic native-read)
(define-generic native-write)
(define-generic compute-standard-fields)
(define-generic compute-native-fields)

(define-macro (assert condition)
  `(or ,condition
       (error "Assertion failed:" ',condition)))

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

(define-class <contacts> ()
  (records #:accessor records #:init-value '()))

(define (map-named-fields record name-map)
  (map (lambda (spec)
	 (cons (car spec)
	       (assoc-ref record (cdr spec))))
       name-map))

;; Import contacts from some native format and write them out in the
;; standard format.
(define (import class source)

  (define (stringify standard-fields)
    ;; Require the standard fields to consist only of list structure
    ;; and string values.
    (cond ((string? standard-fields)
	   standard-fields)
	  ((pair? standard-fields)
	   (if (null? (cdr standard-fields))
	       (list (stringify (car standard-fields)))
	       (cons (stringify (car standard-fields))
		     (stringify (cdr standard-fields)))))
	  ((null? standard-fields)
	   "")
	  (else (error "Bad value in computed standard fields: " standard-fields))))
    
  ;; Make an empty database of the right class.
  (let ((db (make class)))

    ;; Read from the source (a file, a directory, or anything else
    ;; that the db class can understand) into the database.
    (set! (records db)
	  (native-read db source))

    ;; Compute and add standard fields to each record in the database.
    (set! (records db)
	  (map (lambda (record)
		 (append (stringify (compute-standard-fields db record))
			 record))
	       (records db)))

    ;; Delete, one-by-one, any native fields that can be exactly
    ;; reconstructed from the remaining fields.
    (set! (records db)
	  (map (lambda (record)
		 (let deleted-loop ((record record))
		   (let field-loop ((computed-native-fields
				     (compute-native-fields db record)))
		     (if (null? computed-native-fields)
			 record
			 (let ((field-name (caar computed-native-fields)))
			   ;;(write field-name)
			   ;;(newline)
			   ;;(write (cdar computed-native-fields))
			   ;;(newline)
			   ;;(write (assoc-ref record field-name))
			   ;;(newline)
			   (if (equal? (cdar computed-native-fields)
				       (assoc-ref record field-name))
			       (deleted-loop (assoc-remove! record field-name))
			       (field-loop (cdr computed-native-fields))))))))
	       (records db)))

    ;; Write out this set of records in the standard contact format.
    (map write-record (records db))))

;; Write out a single record in the standard contact format.
(define (write-record record)

  ;; Handle writing a single field.
  (define (write-field field)
    (let ((name (car field))
	  (value (cdr field)))
      (if (string=? name "PHONE") ;; Probably add `or "ADDRESSES"'
				  ;; here in future.
	  ;; The value is a list of values with subkeys.
	  (map (lambda (subfield)
		 (let ((subname (car subfield))
		       (subvalue (cadr subfield)))
		   ;; Write out the field name and subname.
		   (format #t "~a ~a~%" name subname)
		   ;; Write out the subvalue.
		   (format #t " ~a~%" subvalue)))
	       value)
	  ;; The value is a single value.
	  (begin
	    ;; Write out the name.
	    (format #t "~a~%" name)
	    ;; Write out the value.
	    (if (string=? (substring name 0 2) "X-")
		(format #t " ~s~%" value)
		(format #t " ~a~%" value))))))

  ;; Compute contact file name from FIRST-NAMES and LAST-NAME.
  (let ((file-name (format #f "_~a_~a"
			   (assoc-ref record "FIRST-NAMES")
			   (assoc-ref record "LAST-NAME"))))

    ;; Ensure the file name is unique.
    (while (file-exists? file-name)
      (set! file-name (string-append file-name "_")))
      
    ;; Write out all this record's fields.
    (with-output-to-file file-name
      (lambda ()
	(for-each write-field record)))))
