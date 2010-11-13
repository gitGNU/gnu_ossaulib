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

(define-module (contacts bbdb)
  #:use-module (contacts common)
  #:use-module (language elisp parser)
  #:use-module (oop goops)
  #:export (<bbdb>))

(define-class <bbdb> (<contacts>))

(define (eol->nil l)
  (if (null? l) #nil l))

;; Read a BBDB file.
(define-method (native-read (db <bbdb>) bbdb-file)

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
  (map (lambda (v-record)
	 `(("X-BBDB-FIRST-NAMES" . ,(vector-ref v-record 0))
	   ("X-BBDB-LAST-NAME"   . ,(vector-ref v-record 1))
	   ("X-BBDB-AKAS"        . ,(vector-ref v-record 2))
	   ("X-BBDB-FIELD-3"     . ,(vector-ref v-record 3))
	   ("X-BBDB-PHONES"      . ,(eol->nil
				     (map vector->list (vector-ref v-record 4))))
	   ("X-BBDB-ADDRESSES"   . ,(eol->nil
				     (map vector->list (vector-ref v-record 5))))
	   ("X-BBDB-EMAILS"      . ,(vector-ref v-record 6))
	   ("X-BBDB-NOTES"       . ,(vector-ref v-record 7))
	   ("X-BBDB-FIELD-8"     . ,(vector-ref v-record 8))))
       vector-data))

;; (native-read (make <bbdb>) "/home/neil/BBDB")

;; Given a BBDB record, compute standard contact fields from it.
(define-method (compute-standard-fields (db <bbdb>) record)
  (cons* (cons "LAST-NAME" (or (assoc-ref record "X-BBDB-LAST-NAME")
			       ""))
	 (map-named-fields record
			   '(("FIRST-NAMES" . "X-BBDB-FIRST-NAMES")
			     ("PHONE"       . "X-BBDB-PHONES")))))

;; Given a set of standard contact fields, compute corresponding
;; native BBDB fields.
(define-method (compute-native-fields (db <bbdb>) record)
  (map (lambda (entry)
	 (set-cdr! entry (eol->nil (cdr entry)))
	 entry)
       (map-named-fields record
			 '(("X-BBDB-FIRST-NAMES" . "FIRST-NAMES")
			   ("X-BBDB-LAST-NAME"   . "LAST-NAME")
			   ("X-BBDB-PHONES"      . "PHONE")))))

;; Write out data in elisp format.
(define (write-elisp x)
  (cond ((vector? x)
	 (display "[")
	 (let ((spacer-proc (make-spacer-proc)))
	   (map (lambda (x)
		  (spacer-proc)
		  (write-elisp x))
		(vector->list x)))
	 (display "]"))
	((list? x)
	 (display " ")
	 (display "(")
	 (let ((spacer-proc (make-spacer-proc)))
	   (map (lambda (x)
		  (spacer-proc)
		  (write-elisp x))
		x))
	 (display ")"))
	((pair? x)
	 (display " ")
	 (display "(")
	 (write-elisp (car x))
	 (display " . ")
	 (write-elisp (cdr x))
	 (display ")"))
	((null? x)
	 (display " ")
	 (display "nil"))
	((string? x)
	 (write x))
	(else
	 (error "Unhandled object: " x))))

(define (make-spacer-proc)
  ;; Return a procedure that does nothing the first time it is called,
  ;; but display's a space on every subsequent call.
  (let ((first-time #t))
    (lambda ()
      (if (not first-time)
	  (display " ")))
    (set! first-time #f)))

;; Write out a database in BBDB format.
(define-method (native-write (db <bbdb>) destination)

  ;; Sorting order for BBDB files.
  (define (bbdb-sort r1 r2)
    ;; Sort algorithm copied from `bbdb-record-sortkey'.
    (string<? (string-downcase
	       (string-append (assoc-ref r1 "X-BBDB-LAST-NAME")
			      (assoc-ref r1 "X-BBDB-FIRST-NAMES")))
	      (string-downcase
	       (string-append (assoc-ref r2 "X-BBDB-LAST-NAME")
			      (assoc-ref r2 "X-BBDB-FIRST-NAMES")))))
    
  (with-output-to-file destination
    (lambda ()
      (set-port-encoding! (current-input-port) "utf-8")
      (display "\
;; -*-coding: utf-8-emacs;-*-
;;; file-version: 6
;;; user-fields: (xmas-card xmas04 mobile\\(julian\\) xmas05 xmas)
")
      (for-each (lambda (record)
		  (write-elisp (vector (assoc-ref record "X-BBDB-FIRST-NAMES")
				       (assoc-ref record "X-BBDB-LAST-NAME"  )
				       (assoc-ref record "X-BBDB-AKAS"       )
				       (assoc-ref record "X-BBDB-FIELD-3"    )
				       (assoc-ref record "X-BBDB-PHONES"     )
				       (assoc-ref record "X-BBDB-ADDRESSES"  )
				       (assoc-ref record "X-BBDB-EMAILS"     )
				       (assoc-ref record "X-BBDB-NOTES"      )
				       (assoc-ref record "X-BBDB-FIELD-8"    )))
		  (newline))
		(sort (records db) bbdb-sort)))))
