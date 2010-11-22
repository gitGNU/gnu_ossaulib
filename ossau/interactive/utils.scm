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

;;; {Interactive Utilities}
;;;
;;; Utility procedures for use in writing interaction handlers.

(define-module (ossau interactive utils)
  #:use-module (ice-9 optargs)
  #:use-module (ossau filesys)
  #:use-module (ossau interactive)
  #:use-module (ossau interactive prompting)
  #:export (interactive-error
	    interactive-read
	    interactive-read-constrained
	    make-interactive-string-reader
	    make-interactive-number-reader
	    interactive-read-using-string))



;; interactive-error EXPRESSION MESSAGE . ARGS
;;
;; Use interactive-error as a standard way of signalling an error in
;; an interactive reading procedure.  interactive-error throws an
;; 'interactive-error key with two arguments: EXPRESSION and a string
;; formed by passing MESSAGE and ARGS to simple-format.

(define (interactive-error expression message . args)
  (throw 'interactive-error
         expression
         (apply simple-format #f message args)))

;; interactive-read PORT PROMPT
;;
;; Call interactive-read to read the next Scheme expression on the
;; interaction port PORT, prompting if necessary with PROMPT.  All
;; expression-based interactive reading procedures should use this as
;; their lowest-level read call.

(define (interactive-read port prompt)
  (set-port-prompts! port prompt "... ")
  (read port))

;; interactive-read-constrained PORT PROMPT CONSTRAIN
;;
;; Read a constrained value interactively.  CONSTRAIN is a procedure
;; of one argument - the expression read, which should either coerce
;; its value to the required type or, if that is impossible, throw an
;; error by calling interactive-error.

(define (interactive-read-constrained port prompt constrain)
  (constrain (interactive-read port prompt)))

;; make-interactive-string-reader PREDICATE DESCRIPTION
;;
;; Return an interactive reading procedure which reads a string that
;; also satisfies PREDICATE; the type of string required is described
;; by DESCRIPTION.  PREDICATE is a procedure of one argument - the
;; string read - that returns non-#f if the string satisfies the
;; required predicate.  The constructed procedure calls
;; interactive-error if the string read does not satisfy PREDICATE.

(define (make-interactive-string-reader predicate description)
  (lambda (port prompt)
    (interactive-read-constrained port prompt
      (lambda (expression)
        (let ((string (cond ((string? expression) expression)
                            ((symbol? expression) (symbol->string expression))
			    ((number? expression) (number->string expression))
                            (#t #f))))
          (if (and string
                   (predicate string))
              string
              (interactive-error expression
                                 (string-append "expected " description))))))))

;; make-interactive-number-reader PREDICATE DESCRIPTION
;;
;; Return an interactive reading procedure which reads a number that
;; also satisfies PREDICATE; the type of number required is described
;; by DESCRIPTION.  PREDICATE is a procedure of one argument - the
;; number read - that returns non-#f if the number satisfies the
;; required predicate.  The constructed procedure calls
;; interactive-error if the number read does not satisfy PREDICATE.

(define (make-interactive-number-reader predicate description)
  (lambda (port prompt . range)
    (let ((min #f)
          (max #f))
      (if (and (pair? range)
               (predicate (car range)))
          (begin
            (set! min (car range))
            (if (and (pair? (cdr range))
                     (predicate (cadr range)))
                (set! max (cadr range)))))
      (interactive-read-constrained port prompt
        (lambda (expression)
          (cond ((and (number? expression)
                      (predicate expression)
                      (or (not min) (>= expression min))
                      (or (not max) (<= expression max)))
                 expression)
                ((and min max)
                 (interactive-error expression
                                    (string-append "expected "
                                                   description
                                                   " in the range ~S..~S")
                                    min
                                    max))
                (min
                 (interactive-error expression
                                    (string-append "expected "
                                                   description
                                                   " greater than or equal to ~S")
                                    min))
                (max
                 (interactive-error expression
                                    (string-append "expected "
                                                   description
                                                   " less than or equal to ~S")
                                    max))
                (#t
                 (interactive-error expression
                                    (string-append "expected "
                                                   description)))))))))

;; interactive-read-using-string SPEC
;;
;; Read arguments immediately according to the interactive
;; specification string SPEC.  The arguments read are returned in a
;; list.

(define* (interactive-read-using-string
	  port
	  spec
	  #:key
	  (context (default-interaction-context)))
  ((interactive-string->interaction-handler spec context) port))
