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

;;; {Interactive String Handlers for Core Data Types}
;;;
;;; Provide a set of interactive string handlers for a core set of
;;; common things that applications like to read, such as strings,
;;; file names, numbers etc.
;;;
;;; Also provide a standard set of associations between these handlers
;;; and interactive string type characters, and an interface that
;;; permits comprehensive or selective installation of these
;;; associations.

(define-module (ossau interactive core-handlers)
  #:use-module (ice-9 optargs)
  #:use-module (ossau filesys)
  #:use-module (ossau interactive)
  #:use-module (ossau interactive utils)
  #:export (interactive-read-string
	    interactive-read-directory-name
	    interactive-read-existing-file-name
	    interactive-read-file-name
	    interactive-read-integer
	    interactive-read-number
	    interactive-read-expression
	    use-core-interactive-string-handlers!))



;;; {Interactive Reading Procedures}
;;;
;;; A set of interactive reading procedures for common "core" data
;;; types.

(define interactive-read-string
  (make-interactive-string-reader (lambda (s) #t) "a string"))

(define interactive-read-directory-name
  (make-interactive-string-reader (lambda (s)
                                    (file-is-directory? (expand-file-name s)))
                                  "an existing directory name"))

(define interactive-read-existing-file-name
  (make-interactive-string-reader (lambda (s)
                                    (file-exists? (expand-file-name s)))
                                  "an existing file name"))

(define interactive-read-file-name
  (make-interactive-string-reader (lambda (s) #t) "a file name"))

(define interactive-read-number
  (make-interactive-number-reader number? "a number"))

(define interactive-read-integer
  (make-interactive-number-reader integer? "an integer"))

(define (interactive-read-expression port prompt)
  (interactive-read port prompt))



;;; {Standard Type Character Associations}
;;;
;;; Registrations associating these handlers with standard type
;;; characters in interactive specification strings.

(define core-type-handler-alist
  `((#\D . ,interactive-read-directory-name)
    (#\f . ,interactive-read-existing-file-name)
    (#\F . ,interactive-read-file-name)
    (#\i . ,interactive-read-integer)
    (#\n . ,interactive-read-number)
    (#\s . ,interactive-read-string)
    (#\x . ,interactive-read-expression)))

;; use-core-interactive-string-handlers! [#:chars CHARS] [#:context CONTEXT]
;;
;; Install some or all of the core interactive string handlers.  These
;; are handlers for the following type characters:
;;   D -- a directory name
;;   f -- an existing file name
;;   F -- a file name that may not exist
;;   i -- an integer
;;   n -- a number
;;   s -- a string
;;   x -- Scheme expression read but not evaluated.
;;
;; If specified, optional keyword argument CHARS indicates the exact
;; set of handlers to install.  The default is to install all of the
;; above.
;;
;; If specified, optional keyword argument CONTEXT indicates the
;; interaction context in which to install these handlers.  The
;; default is to install in the default context for the current
;; dynamic root.
(define* (use-core-interactive-string-handlers!
	  #:key
	  (chars (map car core-type-handler-alist))
	  (context (default-interaction-context)))
  (map (lambda (char)
	 (let ((handler (assq-ref core-type-handler-alist char)))
	   (if handler
	       (set-interactive-string-type-handler! char
						     handler
						     #:context context))))
       chars))
