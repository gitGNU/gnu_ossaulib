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

(define-module (ossau contacts emaillist)
  #:use-module (ossau contacts common)
  #:use-module (oop goops)
  #:export (<emaillist>))

(define-class <emaillist> (<contacts>))

;; Given a set of standard contact fields, compute corresponding
;; native EMAILLIST fields.
(define-method (compute-native-fields (db <emaillist>) record)
  '())

;; Write out a database in EMAILLIST format.
(define-method (native-write (db <emaillist>) destination)
    
  (with-output-to-file destination
    (lambda ()
      (set-port-encoding! (current-output-port) "utf-8")
      (for-each (lambda (record)
		  (let ((name (format #f "~a ~a"
				      (cadr (assoc-ref record 'FIRST-NAMES))
				      (cond ((assoc-ref record 'LAST-NAME)
					     => cadr)
					    (else ""))))
			(emails (with-input-from-string
				    (cond ((assoc-ref record 'X-BBDB-EMAILS)
					   => cadr)
					  (else "()"))
				  read)))
		    ; (format #t ";~s~%" record)
		    (or (null? emails)
			(string? emails)
			(for-each (lambda (email)
				    (format #t "~a <~a>~%" name email))
				  emails))))
		(records db)))))
