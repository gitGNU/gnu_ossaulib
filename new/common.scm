
(define-module (contacts common)
  #:use-module (oop goops)
  #:export (<contacts>
	    records
	    map-named-fields))

(define-class <contacts> ()
  (records #:accessor records #:init-value '()))

(define (map-named-fields record name-map)
  (map (lambda (spec)
	 (cons (car spec)
	       (assq-ref record (cdr spec))))
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
    (read db source)

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
		 (let ((subname (car field))
		       (subvalue (cdr field)))
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
	    (format #t " ~a~%" value))))

    ;; Write out all this record's fields.
    (with-output-to-file (format #f "_~a_~a"
				 (assoc-ref record "FIRST-NAMES")
				 (assoc-ref record "LAST-NAME"))
      (lambda ()
	(for-each write-field record)))))
