;;;;
;;;; 	Copyright (C) 2010 Neil Jerram.
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License as
;;;; published by the Free Software Foundation; either version 3 of
;;;; the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;;; 02110-1301 USA

(define-module (ossau contacts common)
  #:use-module (oop goops)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 string-fun)
  #:export (fold-contacts
	    read-contact
	    <contacts>
	    records
	    map-named-fields
	    native-read
	    native-write
	    compute-standard-fields
	    compute-native-fields
	    import->dir
	    dir->export))

(define-generic native-read)
(define-generic native-write)
(define-generic compute-standard-fields)
(define-generic compute-native-fields)

(define-macro (assert condition)
  `(or ,condition
       (error "Assertion failed:" ',condition)))

(define (fold-contacts dir init proc)
  (let ((filenames '()))
    (nftw dir
	  (lambda (filename statinfo flag base level)
	    (and (eq? flag 'regular)
		 (= level 1)
		 (char=? (string-ref (basename filename) 0) #\_)
		 (set! filenames (cons filename filenames)))
	    #t))
    (for-each (lambda (filename)
		(set! init (proc (with-input-from-file filename read-contact)
				 init)))
	      (sort filenames string<?)))
  init)

(define (read-contact)
  (combine-same-keys (read-contact-1)))

(define (combine-same-keys contact)
  ;; Given a contact as read by `read-contact-1', look for alist
  ;; entries with the same FIELD-KEY and combine them into a single
  ;; entry.  If there was already only a single entry in the original
  ;; alist, with FIELD-IDENTIFIER "", the combined entry is just
  ;; (FIELD-KEY . FIELD-VALUE).  Otherwise the combined entry is
  ;; (FIELD-KEY (FIELD-IDENTIFIER-1 . FIELD-VALUE-1) ...).
  (if (null? contact)
      contact
      (let ((first (car contact)))
	(receive (same-key-list other-key-list)
	    (partition (lambda (entry)
			 (string=? (caar entry) (caar first)))
		       (cdr contact))
	  (acons (caar first)
		 (if (and (null? same-key-list)
			  (string=? (cdar first) ""))
		     (cdr first)
		     (map (lambda (same-key-entry)
			    (cons (cdar same-key-entry)
				  (cdr same-key-entry)))
			  (cons first same-key-list)))
		 (combine-same-keys other-key-list))))))

(define (read-contact-1)
  ;; Read a contact file, in master format, and return it as an alist
  ;;  like (((FIELD-KEY . FIELD-IDENTIFIER) . FIELD-VALUE) ...).
  ;;  FIELD-KEY is a string such as "LAST-NAME", "PHONE" or
  ;;  "X-BBDB-NOTES".  FIELD-IDENTIFIER is a string, such as "Work",
  ;;  "mobile" or "parents' cottage", that differentiates between
  ;;  fields that can have multiple values - like PHONE and ADDRESS.
  ;;  FIELD-VALUE is a string that is the value of the field.
  (let loop ((contact '())
	     (field-key #f)
	     (field-value #f))
    (let ((line (read-line)))
      (cond ((eof-object? line)
	     (reverse! (acons field-key field-value contact)))
	    ((zero? (string-length line))
	     (loop contact field-key field-value))
	    ;; Check for leading space.
	    ((char=? (string-ref line 0) #\space)
	     ;; Start or continuation of a value.
	     (assert field-key)
	     (loop contact
		   field-key
		   (if field-value
		       (string-append field-value "\n" (substring line 1))
		       (substring line 1))))
	    (else
	     ;; A new field, that may or may not have an
	     ;; identifier.
	     (let ((match-data (string-match "^([^ ]+) ?([^ ]*.*)$" line)))
	       (assert match-data)
	       (let ((new-key (match:substring match-data 1)))
		 (loop (if field-key
			   (acons field-key field-value contact)
			   contact)
		       (cons new-key
			     (match:substring match-data 2))
		       (if (string=? (substring new-key 0 2) "X-")
			   (read)
			   #f)))))))))

(define-class <contacts> ()
  (records #:accessor records #:init-value '()))

(define (map-named-fields record name-map)
  (filter cdr
	  (map (lambda (spec)
		 (cons (car spec)
		       (assoc-ref record (cdr spec))))
	       name-map)))

(define standard-fields '("FIRST-NAMES"
			  "LAST-NAME"
			  "PHONE"))

(define (in-standard-order alist)
  (let loop ((fields standard-fields)
	     (input alist)
	     (output '()))
    (if (null? fields)
	(append (reverse! output) input)
	(let* ((field (car fields))
	       (value (assoc-ref input field)))
	  (loop (cdr fields)
		(assoc-remove! input field)
		(if value
		    (acons field value output)
		    output))))))

;; Import contacts from some native format and write them out in
;; standard format to a set of files in the current working directory.
(define (import->dir class source)

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
		 (append (in-standard-order
			  (stringify (compute-standard-fields db record)))
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
    (for-each write-record (records db))))

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
			   (or (assoc-ref record "FIRST-NAMES") "")
			   (or (assoc-ref record "LAST-NAME") ""))))

    ;; Ensure the file name is unique.
    (while (file-exists? file-name)
      (set! file-name (string-append file-name "_")))

    ;; Write out all this record's fields.
    (with-output-to-file file-name
      (lambda ()
	(for-each write-field record)))))

;; Export contacts from the current directory to some native format.
(define (dir->export class target)

  (let ((db (make class)))

    ;; Read standard format contacts from the current directory.
    (set! (records db)
	  ;; Use reverse! here to make import->dir and dir->export
	  ;; symmetric for sets of contacts with the same name.  When
	  ;; there are multiple contacts with the same name, e.g. "X
	  ;; Y": (1) they will be written out in filenames "_X_Y",
	  ;; "_X_Y_", "_X_Y__", and so on; (2) nftw will traverse them
	  ;; in that same order - i.e. a name with an extra underscore
	  ;; will come after a name without; (3) therefore the
	  ;; following use of cons will construct a list where those
	  ;; contacts are in reversed order; (4) calling reverse! will
	  ;; correct that.
	  (reverse! (fold-contacts "." '() cons)))

    ;; Compute additional native fields from the standard fields.
    (set! (records db)
	  (map (lambda (record)
		 (let loop ((computed-fields
			     (compute-native-fields db record))
			    (record record))
		   (if (null? computed-fields)
		       record
		       (loop (cdr computed-fields)
			     (if (assoc-ref record (caar computed-fields))
				 record
				 (cons (car computed-fields) record))))))
	       (records db)))
    
    ;; Write records out in native format.
    (native-write db target)))
