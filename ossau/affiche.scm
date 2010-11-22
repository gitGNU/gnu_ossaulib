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

(define-module (ossau affiche)
  #:use-module (ice-9 buffered-input)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ossau interactive)
  #:use-module (ossau interactive prompting)
  #:use-module (ossau interactive utils)
  #:use-module (ossau interactive core-handlers)
  #:use-module (ossau command-loop)
  #:use-module (ossau filesys)
  #:export (affiche
	    clear
	    echo
	    magnification
	    prefix
	    info
	    describe
	    data
	    matrix
	    rpn
	    rpn-prepare
	    rpn-operate
	    ignore
	    set
	    return))

(define *data* '())
(define *graphs* '())

(define *magnification* 1)

(define *current-prefix* "")

(define *variables* '())

(define *verbose* #f)

(use-core-interactive-string-handlers!)

;; interactive-read-data-name PROMPT
;;
;; Read the name of an existing Affiche vector or matrix.

(define interactive-read-data-name
  (make-interactive-string-reader (lambda (s)
                                    (assoc s *data*))
                                  "an existing vector or matrix name"))

(set-interactive-string-type-handler! #\d interactive-read-data-name)

;; interactive-read-object-name PROMPT
;;
;; Read and return the name of an existing Affiche object.

(define interactive-read-object-name
  (make-interactive-string-reader (lambda (s)
                                    (or (assoc s *data*)
                                        (assoc s *graphs*)))
                                  "an existing Affiche data or graph object"))

(set-interactive-string-type-handler! #\o interactive-read-object-name)

;; interactive-read-object PROMPT
;;
;; Read the name of an existing Affiche object and return the
;; underlying object.

(define (interactive-read-object port prompt)
  (let ((name (interactive-read-object-name port prompt)))
    (or (assoc-ref *data* name)
        (assoc-ref *graphs* name))))

(set-interactive-string-type-handler! #\O interactive-read-object)

;; affiche [FILE-NAME]
;;
;; Run Affiche, with input from FILE-NAME if specified, or otherwise
;; from standard input.

(define (affiche . files)
  (if (null? files)
      (run-command-loop "[] "
			#:command-module (resolve-module '(ossau affiche))
			#:with-standard-commands '(help return quit)
			;#:gobble-rest-of-line #t
			#:input-port
			(make-affiche-input-port (current-input-port)
						 (current-output-port)))
      (with-input-from-file (expand-file-name (car files)) affiche)))

(set-procedure-interactive-string! affiche "fRun Affiche file: ")

;; bye
;;
;; Exit Affiche.

(define bye exit)

;; clear
;;
;; Return to initial state, deleting all Affiche objects and
;; variables.

(define (clear)
  (dset! *magnification* 1)
  (set! *current-prefix* "")
  (set! *variables* '())
  (set! *data* '())
  (set! *graphs* '()))

(set-procedure-interactive-string! clear "")

;; echo STRING
;;
;; Echo the specified string (useful in scripts).

(define (echo string)
  (display string)
  (newline))

(set-procedure-interactive-string! echo "sString: ")

;; magnification MAG
;;
;; Set default magnification to MAG.

(define (magnification mag)
  (set! *magnification* mag)
  (display "Default magnification = ")
  (display *magnification*)
  (newline))

(set-procedure-interactive-string! magnification "nMagnification: ")

;; prefix PREF
;;
;; Set current prefix to PREF.

(define (prefix pref)
  (set! *current-prefix* pref))

(set-procedure-interactive-string! prefix "sPrefix: ")

;; info
;;
;; Show current values of all variables.

(define (info)
  (display "Current prefix = ")
  (write *current-prefix*)
  (newline)
  (display "Default magnification = ")
  (write *magnification*)
  (newline)
  (display "Verbose = ")
  (write *verbose*)
  (newline)
  ;; @@@ ignore_file_errors
  ;; @@@ current_file_name
  ;; @@@ quiet
  ;; @@@ tty
  ;; @@@ variables
  (display "\nVariables:\n")
  (for-each (lambda (var+value)
	      (format #t "$~A => ~A\n" (car var+value) (cdr var+value)))
	    (reverse *variables*))
  (display "\nData:\n")
  (for-each describe (map cdr *data*))
  ;; @@@ graphs
  )

(set-procedure-interactive-string! info "")

;; describe OBJ
;;
;; Describe the Affiche object OBJ.

(define (describe obj)
  (cond
   ((array? obj)
    (let* ((name (object-property obj 'name))
           (dims (array-dimensions obj))
           (rank (length dims)))
      (case rank
        ((1) (simple-format #t
                            "Vector ~S length ~S\n"
                            name
                            (list-ref dims 0)))
        ((2) (simple-format #t
                            "Matrix ~S dimensions ~Sx~S\n"
                            name
                            (list-ref dims 0)
                            (list-ref dims 1)))
        (else (let loop ((dims-string (simple-format #f
                                                     "~S"
                                                     (car dims)))
                         (dims (cdr dims)))
                (if (null? dims)
                    (simple-format #t
                                   "Data ~S dimensions ~S\n"
                                   name
                                   dims-string)
                    (loop (simple-format #f
                                         "~Sx~S"
                                         dims-string
                                         (car dims))
                          (cdr dims))))))))

   (#t
    (display "Unknown object: ")
    (write obj)
    (newline))))

(set-procedure-interactive-string! describe "OAffiche object: ")

;; data NAME D
;;
;; Create an Affiche data object with name NAME and data D.

(define (data name d)
  (set-object-property! d 'name name)
  (set! *data* (assoc-set! *data* name d))
  (describe d))

;; remove NAME
;;
;; Remove the Affiche data or graph object with name NAME.

(define (remove name)
  (set! *data* (assoc-remove! *data* name))
  (set! *graphs* (assoc-remove! *graphs* name)))

(set-procedure-interactive-string! remove "oAffiche object: ")

;; linear-vector NAME LENGTH MIN MAX
;;
;; Create a linearly varying vector from MIN to MAX with length
;; LENGTH.

(define (linear-vector name length min max)
  (let ((lv (dimensions->uniform-array (list length) 1.0 0.0))
	(step (/ (- max min) (- length 1))))
    (do ((i 0 (+ i 1)))
	((= i length) #f)
      (uniform-vector-set! lv i (+ min (* i step))))
    (data name lv)))

(set-procedure-interactive-string! linear-vector "sVector name: \niVector length: \nnFrom (min value): \nnTo (max value): ")

;; vector NAME DATA
;;
;; Create an Affiche vector with name NAME and data D.

(define (vector name d)
  (data name d))

(set-procedure-interaction-handler! vector
  (lambda (port)
    (let* ((name (interactive-read-string port "Vector name: "))
           (length (interactive-read-integer port "Vector length: " 1 #t))
           (data (dimensions->uniform-array (list length) 1.0 0.0)))
      (list name
            (do ((i 0 (+ i 1)))
                ((= i length) data)
              (uniform-vector-set! data i
                                   (interactive-read-number
				    port
                                    (simple-format #f
                                                   "Vector data (~A/~A): "
                                                   (+ i 1)
                                                   length)
                                    #t #t)))))))

;; matrix NAME DATA
;;
;; Create an Affiche matrix with name NAME and data D.

(define (matrix name d)
  (data name d))

(set-procedure-interaction-handler! matrix
  (lambda (port)
    (let* ((name (interactive-read-string port "Matrix name: "))
           (length1 (interactive-read-integer port "First matrix dimension: " 1 #t))
           (length2 (interactive-read-integer port"Second matrix dimension: " 1 #t))
           (data (dimensions->uniform-array (list length1 length2) 1.0 0.0)))
      (list name
            (do ((j 0 (+ j 1)))
                ((= j length2) data)
              (do ((i 0 (+ i 1)))
                  ((= i length1) data)
                (array-set! data
                            (interactive-read-number
			     port
                             (simple-format #f
                                            "Matrix data (~A/~A,~A/~A): "
                                            (+ i 1)
                                            length1
                                            (+ j 1)
                                            length2)
                             #t #t)
                            i
                            j)))))))

;; rpn HSILOP DATA
;;
;; Apply reverse Polish notation HSILOP to DATA.

(define (rpn hsilop data)
  (rpn-prepare hsilop)
  (rpn-operate data))

(set-procedure-interactive-string! rpn "sReverse Polish notation: \ndData name: ")

;; rpn-prepare HSILOP
;;
;; Parse the reverse Polish notation HSILOP.

(define (rpn-prepare hsilop)
  hsilop)

(set-procedure-interactive-string! rpn-prepare "sReverse Polish notation: ")

;; rpn-operate DATA
;;
;; Apply the last parsed reverse Polish notation to DATA.

(define (rpn-operate data)
  data)

(set-procedure-interactive-string! rpn-operate "dData name: ")

;; version
;;
;; Return Guile/Affiche version string.

(define (version)
  "Guile/Affiche version 0.0")

(set-procedure-interactive-string! version "")

;; ignore
;;
;; Hack for Affiche comment lines beginning with `# '.

(read-hash-extend #\space
		  (lambda (c port)
		    'ignore))

(define (ignore)
  #f)

(set-procedure-interactive-string! ignore "")

;; set
;;
;; Textual substitution variables.

(define (set var value)
  (set! *variables* 
	(sort (assoc-set! *variables* var value)
	      (lambda (v1 v2)
		(string>? (symbol->string (car v1)) (symbol->string (car v2)))))))

(set-procedure-interactive-string! set "xVariable: \nsValue: ")

(define translate
  (let ((in-string #f))
    (lambda (s)
      (if (and (not in-string)
	       (string-match "^ *#" s))
	  ;; A comment: return empty string.
	  ""
	  (let loop ((translated "") (s s))
	    (if (string-null? s)
;		(substitute-variables translated)
		translated
		(let ((sq (string-index s #\'))
		      (dq (string-index s #\")))
		  (cond ((and sq (or (not dq) (< sq dq)))
			 ;; Single quote occurs first.
			 (cond ((eq? in-string #\')
				;; We were in a single quoted string.
				;; This is the end of it.
				(set! in-string #f)
				(loop (string-append translated
						     (substring s 0 sq)
						     "\"")
				      (substring s (+ sq 1))))
			   
			       ((eq? in-string #\")
				;; We are in a double quoted string.
				;; Turn this single quote into a
				;; quoted double quote.
				(loop (string-append translated
						     (substring s 0 sq)
						     "\\\"")
				      (substring s (+ sq 1))))

			       (else
				;; We were not in a string.  We are
				;; now.
				(set! in-string #\')
				(loop (string-append translated
						     (substring s 0 sq)
						     "\"")
				      (substring s (+ sq 1))))))

			(dq
			 ;; Double quote occurs first.
			 (cond ((eq? in-string #\")
				;; We were in a double quoted string.
				;; This is the end of it.
				(set! in-string #f)
				(loop (string-append translated
						     (substring s 0 dq)
						     "\"")
				      (substring s (+ dq 1))))
			   
			       ((eq? in-string #\')
				;; We are in a single quoted string.
				;; Turn this double quote into a
				;; quoted double quote.
				(loop (string-append translated
						     (substring s 0 dq)
						     "\\\"")
				      (substring s (+ dq 1))))

			       (else
				;; We were not in a string.  We are
				;; now.
				(set! in-string #\")
				(loop (string-append translated
						     (substring s 0 dq)
						     "\"")
				      (substring s (+ dq 1))))))

			(else
			 ;; Remaining string contains no quotes.
			 (loop (string-append translated s) ""))))))))))

(define (substitute-variables s)
  (let loop ((substituted "") (s s))
    (if (string-null? s)
	substituted
	(let ((dollar (string-index s #\$)))
	  (if dollar
	      (let vlp ((vars *variables*))
		(if (null? vars)
		    ;; No matching variable, so no substitution.
		    (loop (string-append substituted s) "")
		    (let* ((var (symbol->string (caar vars)))
			   (val (cdar vars))
			   (varlen (string-length var)))
		      (if (and (> (string-length s)
				  (+ dollar varlen))
			       (string=? (substring s (+ dollar 1) (+ dollar 1 varlen))
					 var))
			  ;; This variable matches.
			  (loop (string-append substituted
					       (substring s 0 dollar)
					       val)
				(substring s (+ dollar 1 varlen)))
			  ;; This variable doesn't match: try the next.
			  (vlp (cdr vars))))))
	      (loop (string-append substituted s) ""))))))

(define (make-affiche-input-port input-port output-port)
  (make-prompting-input-port input-port
			     output-port
			     (lambda (line)
			       (if (eof-object? line)
				   line
				   (translate (substitute-variables line))))))
