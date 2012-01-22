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

(define-module (ossau command-loop)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 documentation)
  #:use-module (ice-9 common-list)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 stack-catch)
  #:use-module (ossau interactive)
  #:use-module (ossau interactive utils)
  #:use-module (ossau interactive prompting)
  #:use-module (ossau interactive core-handlers)
  #:export (run-command-loop
	    command-loop-quit
	    command-loop-error))


;; Compatibility
(cond-expand (guile-2 'not-needed)
	     (else
	      (if (and (defined? 'call-with-readline-completion-function)
		       (not (defined? 'with-readline-completion-function)))
		  (define with-readline-completion-function
		    call-with-readline-completion-function))))

;; run-command-loop PROMPT [#:commands COMMANDS]
;;                         [#:command-module COMMAND-MODULE]
;;                         [#:with-standard-commands STDCMDS]
;;                         [#:gobble-rest-of-line GOBBLE?]
;;                         [#:unknown-command-handler UC-HANDLER]
;;                         [#:bad-expression-handler BE-HANDLER]
;;
;; Run a command loop with primary prompt PROMPT.
;;
;; `run-command-loop' repeatedly reads a command name and then calls
;; the corresponding command procedure using `call-interactively!'.
;;
;; The commands that are available in the command loop are defined by
;; either COMMANDS or COMMAND-MODULE, only one of which should be
;; specified.
;;
;; If COMMANDS is specified, it should be a list of procedures, each
;; of which has an interactive calling specification (see
;; `procedure-interactive?').  The command name for each procedure
;; PROC - i.e. what the user has to type for that command - is taken
;; to be `(procedure-name PROC)'.
;;
;; If COMMAND-MODULE is specified, the list of command procedures is
;; constructed automatically by selecting all the bindings from the
;; specified module whose values are interactive procedures.
;;
;; If STDCMDS is specified, it should be a list of symbols taken from
;; the following set, and indicates that the following standard
;; commands should automatically be made available within the command
;; loop.
;;
;;     help       - When typed without an argument, the standard
;;                  `help' command prints a list of all available
;;                  commands.  When typed with an argument that
;;                  matches one of the available command names,
;;                  `help' prints the procedure documentation for
;;                  that command.
;;
;;     quit       - The standard `quit' command causes
;;                  `run-command-loop' to return to its caller
;;                  with an unspecified return value.
;;
;;     return     - The standard `return' command requires a Scheme
;;                  expression as its argument.  It evaluates that
;;                  expression and causes `run-command-loop' to
;;                  return to its caller with the value obtained.
;;
;; If specified and not `#f', GOBBLE? indicates that extraneous input
;; at the end of the command loop should be ignored.  If set to
;; `#:warn', the command loop also prints a warning message; any
;; other non-`#f' value causes the input to be ignored without any
;; warning.  If GOBBLE? is not specified, or set to `#f', any
;; apparently extraneous input is left in the input buffer to be
;; interpreted as the start of the next command.
;;
;; If UC-HANDLER is specified and not `#f', it should be a procedure
;; that accepts one argument: the command symbol that
;; `run-command-loop' didn't recognize.  If the procedure returns
;; `#f', `run-command-loop' raises a command loop error with its usual
;; error message for an unrecognized command; if the procedure returns
;; non-`#f', `run-command-loop' assumes that the command has been
;; processed and takes no further action.
;;
;; If BE-HANDLER is specified and not `#f', it should be a procedure
;; that accepts one argument: the Scheme expression that
;; `run-command-loop' didn't understand.  If the procedure returns
;; `#f', `run-command-loop' raises a command loop error with its usual
;; error message for a bad expression; if the procedure returns
;; non-`#f', `run-command-loop' assumes that the expression has been
;; processed and takes no further action.
;;
(define* (run-command-loop prompt
			   #:key
			   (command-module #f)
			   (commands #f)
			   (with-standard-commands '())
			   (gobble-rest-of-line #f)
			   (unknown-command-handler #f)
			   (bad-expression-handler #f)
			   (input-port (current-input-port)))

  ;; Define utility reader proc.
  (define (command-reader)
    (set-port-prompts! input-port prompt "... ")
    (read input-port))

  ;; If the set of commands is specified by module, build the command
  ;; list by finding all the interactive procedures in that module.
  (if command-module
      (let ((module->commands
	     (lambda (module)
	       (pick-mappings (lambda (value)
				(and (procedure? value)
				     (procedure-name value)
				     (procedure-interactive? value)
				     value))
			      (module-map (lambda (sym var)
					    (and (variable-bound? var)
						 (variable-ref var)))
					  module)))))
	(if commands
	    (error "Cannot specify both command-module and commands options."))
	(set! commands
	      (if (list? command-module)
		  (uniq (apply append! (map module->commands command-module)))
		  (module->commands command-module)))))

  ;; Check that some commands were specified.
  (or (and commands
	   (not (null? commands)))
      (error "No commands specified for command loop!"))

  ;; For later convenience, map the command list into an alist whose
  ;; keys are procedure name symbols.
  (set! commands (map (lambda (proc)
			(cons (procedure-name proc) proc))
		      commands))

  ;; Add in standard help command if so requested.
  (if (memq 'help with-standard-commands)
      (let ((help (lambda (symbol)
		    "Type `help' followed by a command name for full documentation."
		    (if (symbol? symbol)
			(let ((command-entry (assq symbol commands)))
			  (if command-entry
			      (display (or (object-documentation (cdr command-entry))
					   "Undocumented."))
			      (format #t "`~S' is not a known command" symbol))
			  (newline))
			(begin
			  (display "Type `help' followed by a command name for full documentation.\n")
			  (display "Available commands are:\n")
			  (for-each (lambda (command-entry)
				      (or (eq? (car command-entry) 'help)
					  (format #t "\t~A\n" (car command-entry))))
				    commands))))))
	(set-procedure-interactive-string! help "?x")
	(set! commands (acons 'help help commands))))

  ;; Ditto standard quit command.
  (if (memq 'quit with-standard-commands)
      (let ((quit (lambda ()
		    "Exit the command loop (and return to its caller)."
		    (command-loop-quit))))
	(set-procedure-interactive-string! quit "")
	(set! commands (acons 'quit quit commands))))

  ;; Ditto standard return command.
  (if (memq 'return with-standard-commands)
      (let ((return (lambda (value)
		      "Exit the command loop, returning @var{value} to its caller."
		      (command-loop-quit value)))
	    (handler (lambda (port prompt)
		       (eval (interactive-read port prompt)
			     (interaction-environment)))))
	(set-interactive-string-type-handler! #\X handler)
	(set-procedure-interactive-string! return "XReturn value: ")
	(set! commands (acons 'return return commands))))

  ;; Sort the command alist in alphabetical order.
  (set! commands (sort commands
		       (lambda (proc1 proc2)
			 (string<? (symbol->string (car proc1))
				   (symbol->string (car proc2))))))

  ;; Kick off the command loop.
  (with-readline-completion-function (command-completion-function commands)
    (lambda ()

      (let loop ((expression (command-reader)))

	(let ((continue (list 'continue))

	      (maybe-gobble-rest-of-line
	       (lambda ()
		 (if (and gobble-rest-of-line
			  (current-line-input-available input-port))
		     (let ((rol (rest-of-line-input input-port)))
		       (if (eq? gobble-rest-of-line
				#:warn)
			   (format #t
				   "\n(Ignored extra input `~A' at end of command line.)\n"
				   rol))))))

	      (handle-interactive-error
	       (lambda (key argexp message)
		 (command-loop-error 'arg-error
				     "Bad expression `~S' while reading arguments for `~S':\n  ~A"
				     argexp
				     expression
				     message)))

	      (handle-unknown-command-error
	       (lambda ()
		 (or (and unknown-command-handler
			  (unknown-command-handler expression))
		     (command-loop-error 'command-error
					 "`~S' is not a known command"
					 expression))))

	      (handle-bad-expression-error
	       (lambda ()
		 (or (and bad-expression-handler
			  (bad-expression-handler expression))
		     (command-loop-error 'command-error
					 "`~S' is not a valid command expression"
					 expression)))))
	  
	  ;; Using `stack-catch' here means that errors go through
	  ;; `lazy-handler-dispatch'.  (Which means that a
	  ;; debug-on-error implementation that works by redefining
	  ;; `lazy-handler-dispatch' will automatically apply to this
	  ;; command loop as well.)
	  (let ((result (stack-catch #t

				     (lambda ()
				       (cond ((eof-object? expression)
					      (command-loop-quit))
					     ((symbol? expression)
					      (let ((proc (assq-ref commands expression)))
						(if proc
						    (catch 'interactive-error
						      (lambda ()
							(call-interactively! proc input-port)
							(maybe-gobble-rest-of-line))
						      handle-interactive-error)
						    (handle-unknown-command-error))))
					     (else
					      (handle-bad-expression-error)))
				       continue)

				     (lambda (key . args)
				       (case key
					 ((command-loop-quit)
					  (if (null? args)
					      *unspecified*
					      (car args)))
					 ((command-loop-error)
					  (display (case (car args)
						     ((command-error) "Command error: ")
						     ((arg-error) "Argument error: ")
						     ((error) "Error: ")
						     (else (format #f "Strange (~S) command loop error: "
								   (car args)))))
					  (display (cadr args))
					  (newline)
					  continue)
					 (else
					  (format #t "ERROR (~S):" key)
					  (let loop2 ((args args))
					    (or (null? args)
						(begin
						  (display " ")
						  (display (car args))
						  (loop2 (cdr args)))))
					  (newline)
					  continue))))))
	    (if (eq? result continue)
		(loop (command-reader))
		result)))))))

;; command-loop-quit [VALUE]
;;
;; Quit the command loop, returning VALUE to its caller.
;; If VALUE is omitted, return value is unspecified.
(define (command-loop-quit . args)
  (apply throw 'command-loop-quit args))

;; command-loop-error SUBKEY MESSAGE . ARGS
;;
;; Raise a command loop error with subkey SUBKEY and explanatory
;; message formed by passing MESSAGE and ARGS to simple-format.
;;
;; command-loop-error is intended for use by command procedures when
;; they encounter an error and need to jump back to the top level of
;; the command loop.
(define (command-loop-error subkey message . args)
  (throw 'command-loop-error
         subkey 
         (apply simple-format #f message args)))

(define (command-completion-function commands)
  (letrec ((cmds '())
	   (regexp #f)
	   (completer (lambda (text continue?)
			(if continue?
			    (if (null? cmds)
				#f
				(let ((cmd (symbol->string (caar cmds))))
				  (set! cmds (cdr cmds))
				  (if (string-match regexp cmd)
				      cmd
				      (completer text #t))))
			    (begin
			      (set! cmds commands)
			      (set! regexp
				    (string-append "^" (regexp-quote text)))
			      (completer text #t))))))
    completer))
