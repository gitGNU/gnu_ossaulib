;;;; interactive.scm --- support for reading arguments interactively
;;;;
;;;;    Copyright (C) 2001 Neil Jerram (neil@ossau.uklinux.net)
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA



;;; {Obtaining Arguments Interactively}
;;;
;;; A common requirement in interactive systems that use Guile is the
;;; ability to link an event - such as a mouse click or a key press -
;;; with an arbitrary Guile procedure, such that the procedure will be
;;; run automatically when the appropriate event occurs.
;;;
;;; Most Guile procedures require arguments, however.  Before the
;;; procedure can be run, an appropriate set of arguments must be
;;; constructed, either by deducing them from the current application
;;; environment, or by querying the user, or a combination of the two.
;;; This module provides a framework for procedure authors/users which
;;; allows them to specify how the argument list should be constructed
;;; in an interactive environment.

(define-module (ossau interactive)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 string-fun)
  #:use-module (ossau filesys)
  #:export (default-interaction-context
	    make-interaction-context
	    set-procedure-interaction-handler!
	    call-interactively!
	    procedure-interactive?
	    set-procedure-interactive-string!
	    set-interactive-string-special-handler!
	    set-interactive-string-type-handler!
	    interactive-string->interaction-handler
	    current-line-input-available
	    rest-of-line-input
	    make-optional-handler))



;;; {Interaction Contexts}
;;;
;;; In a multitasking Guile system, interaction contexts are used to
;;; separate the interaction requirements of one part of the system
;;; from those of another.  An interaction context can include all
;;; kinds of information pertinent to how interaction is done in that
;;; context, for example the ports to use for input and output, or the
;;; special and type character handlers that are installed for
;;; interactive strings in that context.
;;;
;;; By default, each dynamic root has its own interaction context, and
;;; this default is understood whenever the optional context argument
;;; is omitted on the procedures below.  More customized behaviour can
;;; be obtained by using `make-interaction-context' to make a new
;;; interaction context and then passing this to the relevant
;;; procedures.

(define *interaction-context* (make-fluid))

;; default-interaction-context
;;
;; Return the default interaction context for the current dynamic
;; root.
(define (default-interaction-context)
  (cond ((fluid-ref *interaction-context*))
	(else
	 (let ((context (make-interaction-context)))
	   (fluid-set! *interaction-context* context)
	   context))))

;; make-interaction-context
;;
;; Make and return a new interaction context.
(define (make-interaction-context)
  ;; A new context consists of an initially empty alist of special
  ;; character handlers and an initially empty alist of type character
  ;; handlers.
  (list (list) (list)))



;;; {Interaction Handlers}
;;;
;;; A procedure can have an "interaction handler" associated with it,
;;; which is responsible for supplying a list of arguments when that
;;; procedure is called interactively.  The interaction handler is a
;;; procedure of one argument, the interaction context, that returns a
;;; list of arguments.
;;;
;;; When a procedure is invoked interactively using
;;; call-interactively!, call-interactively! first calls the
;;; procedure's interaction handler to obtain a list of arguments, and
;;; then applies the procedure to those arguments.

(define procedure-interaction-handler (make-object-property))

;; set-procedure-interaction-handler! PROC HANDLER
;;
;; Set the interaction handler for procedure PROC to HANDLER.  HANDLER
;; is a procedure of one argument, the interaction input port, that
;; should return the list of arguments to be applied to PROC when PROC
;; is called interactively.

(define (set-procedure-interaction-handler! proc handler)
  (set! (procedure-interaction-handler proc) handler))

;; call-interactively! PROC PORT
;;
;; Call procedure PROC interactively.  PROC must be a procedure with
;; an interaction handler (set up using
;; set-procedure-interaction-handler! or
;; set-procedure-interactive-string!).  call-interactively! calls the
;; handler thunk with one argument, the interaction input PORT, and
;; applies the list of arguments that the handler returns to PROC.

(define* (call-interactively! proc port)
  (let ((handler (procedure-interaction-handler proc)))
    (if handler
        (apply proc (handler port))
        (error "Procedure cannot be called interactively:" proc))))

;; procedure-interactive? PROC
;;
;; Test whether procedure PROC can be called interactively.

(define (procedure-interactive? proc)
  (procedure-interaction-handler proc))



;;; {Emacs-style `interactive' Strings}
;;;
;;; In many common cases, it is convenient to specify how arguments
;;; should be obtained interactively using Emacs-style `interactive'
;;; strings.  set-procedure-interactive-string! accepts an interactive
;;; specification string and uses it to construct an interaction
;;; handler for the specified procedure.
;;;
;;; Emacs interactive string specifications consist of a number of
;;; <type-character><prompt-string> specifications, one for each
;;; argument to be specified, separated by newline characters.  The
;;; <type-character> tells Emacs what sort of argument it is expecting
;;; and how to read it or deduce it from the environment.  In
;;; addition, there are a number of special characters, that are only
;;; special if they occur at the beginning of the interactive
;;; specification, which tell Emacs to do/check certain things before
;;; it starts building the argument list.
;;;
;;; Most of the type and special characters that Emacs uses are
;;; meaningless to core Guile, since they refer to specific
;;; editing/Emacs concepts like the current buffer and the numerical
;;; prefix argument.  So it doesn't make sense to try and implement
;;; all of these type and special characters here.  Instead we
;;; implement the small number of type characters that describe
;;; non-application-specific concepts, and allow Guile-based
;;; applications to extend the set of supported characters by
;;; registering handlers for them.

;; set-procedure-interactive-string! PROC SPEC
;;
;; Set the interaction handler for procedure PROC to a handler
;; constructed according to the Emacs-style interactive specification
;; SPEC.

(define* (set-procedure-interactive-string!
	  proc
	  spec
	  #:key
	  (context (default-interaction-context)))
  (set-procedure-interaction-handler!
   proc
   (interactive-string->interaction-handler spec context)))

;; set-interactive-string-special-handler! CHAR HANDLER [#:context CONTEXT]
;;
;; Set the handler procedure for the interactive string special
;; character CHAR to HANDLER.
;;
;; HANDLER must be a procedure of one argument, an input port, that
;; does I/O if necessary via that port.  If HANDLER is #f, any
;; existing handler for CHAR as a special character is removed.
;;
;; Optional keyword argument CONTEXT specifies the interaction context
;; in which to make this change.  Each dynamic root has its own
;; default interaction context; if CONTEXT is omitted, the change is
;; made in the default context for the current dynamic root.
(define* (set-interactive-string-special-handler!
	  char
	  handler
	  #:key
	  (context (default-interaction-context)))
  (if (and (char? char)
	   (or (eq? handler #f)
	       (procedure? handler)))
      (set-car! context
		(if handler
		    (assq-set! (car context) char handler)
		    (assq-remove! (car context) char)))
      (error "Bad arguments")))

;; set-interactive-string-type-handler! CHAR HANDLER [#:context CONTEXT]
;;
;; Set the handler procedure for the interactive string type character
;; CHAR to HANDLER.
;;
;; HANDLER must be a procedure of two arguments, an input port and a
;; prompt, that does I/O if necessary via that port.  If HANDLER is
;; #f, any existing handler for CHAR as a special character is
;; removed.
;;
;; Optional keyword argument CONTEXT specifies the interaction context
;; in which to make this change.  Each dynamic root has its own
;; default interaction context; if CONTEXT is omitted, the change is
;; made in the default context for the current dynamic root.
(define* (set-interactive-string-type-handler!
	  char
	  handler
	  #:key
	  (context (default-interaction-context)))
  (if (and (char? char)
	   (or (eq? handler #f)
	       (procedure? handler)))
      (set-car! (cdr context)
		(if handler
		    (assq-set! (cadr context) char handler)
		    (assq-remove! (cadr context) char)))
      (error "Bad arguments:" char handler)))

;; interactive-string->interaction-handler S CONTEXT
;;
;; This procedure converts an Emacs-style interactive string
;; specification to an interaction handler procedure, using the two
;; alists above to map type and special characters to procedures that
;; can read particular arguments.
;;
;; Note that the alists are consulted when the interaction handler is
;; created, not when it is used.  Subsequent changes to the alists
;; will therefore not affect existing interaction handlers.

(define (interactive-string->interaction-handler s context)

  ;; First check for special characters at the beginning of the
  ;; interactive string.  Special handler procedures are accumulated in
  ;; special-handlers.
  (let special-loop ((s s) (special-handlers '()))
    (let ((special-handler (and (> (string-length s) 0)
                                (assq-ref (car context)
                                          (string-ref s 0)))))
      (if special-handler
          (special-loop (substring s 1)
                        (append special-handlers (list special-handler)))
          
          ;; Now divide the rest of the string at newline characters,
          ;; and accumulate handlers and prompts for the argument
          ;; specifiers.
          (let arg-loop ((arg-handlers '())
                         (prompts '())
                         (arg-fields (separate-fields-discarding-char
                                      #\nl s list)))
            (if (not (null? arg-fields))
		(let ((field (car arg-fields))
		      (optional? #f))

		  ;; Check for an initial ? indicating that this
		  ;; argument is optional.
		  (if (and (> (string-length field) 0)
			   (char=? (string-ref field 0) #\?))
		      (begin
			(set! field (substring field 1))
			(set! optional? #t)))

		  ;; Check that this field contains at least a type
		  ;; character.
		  (if (> (string-length field) 0)

		      ;; Get the handler for the next argument.
		      (let ((arg-handler (assq-ref (cadr context)
						   (string-ref field 0))))
			(if arg-handler
			    (arg-loop (append arg-handlers
					      (list (if optional?
							(make-optional-handler
							   arg-handler)
							arg-handler)))
				      (append prompts
					      (list (substring field 1)))
				      (cdr arg-fields))

			    ;; Argument handler not found!
			    (error "No handler registered for type character"
				   (string-ref field 0))))

		      ;; This field is empty - ignore it.
		      (arg-loop arg-handlers prompts (cdr arg-fields))))

                ;; We've reached the end of interactive string, so now
                ;; we can use special-handlers, arg-handlers and
                ;; prompts to build the interaction handler that will
                ;; be invoked when this interactive procedure is
                ;; called.
                (lambda (port)

                  ;; Call the special handlers - ignore anything that
                  ;; they might return.
                  (map (lambda (handler)
                         (handler port))
                       special-handlers)

                  ;; Call the argument handlers and return a list of
                  ;; their return values.
                  (map (lambda (handler prompt)
                         (handler port prompt))
                       arg-handlers prompts))))))))

;; current-line-input-available
;;
;; Return #t if any non-whitespace input is left on the current input
;; line; otherwise #f.
(define (current-line-input-available port)
  (let eat-whitespace ((ch (peek-char port)))
    (cond ((eof-object? ch) #f)
	  ((char=? ch #\newline) #f)
	  ((char-whitespace? ch)
	   (read-char port)
	   (eat-whitespace (peek-char port)))
	  (else #t))))

;; rest-of-line-input
;;
;; Return the remaining input on the current line as a string.
(define (rest-of-line-input port)
  (list->string
   (reverse!
    (let loop ((input '()) (ch (peek-char port)))
      (cond ((eof-object? ch) input)
	    ((char=? ch #\newline) input)
	    (else
	     (read-char port)
	     (loop (cons ch input) (peek-char port))))))))

;; make-optional-handler HANDLER
;;
;; Given an interaction handler, construct and return a wrapper
;; handler that only reads its argument if that argument is already
;; present on the current input line; otherwise it returns #f.
(define (make-optional-handler handler)
  (lambda (port prompt)
    (and (current-line-input-available port)
	 (handler port prompt))))
