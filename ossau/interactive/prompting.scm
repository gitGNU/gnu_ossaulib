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

;;; {Input Prompting}
;;;
;;; The premise of this module is that the prompt that an input port
;;; gives to the user when it wants more input should be a property of
;;; that input port, and that the application that uses an input port
;;; fundamentally cannot know when it is necessary to give a prompt.
;;; Therefore, the application should content itself with telling the
;;; port what prompt to give in the event that it is needed, and the
;;; port itself should handle the mechanics of giving the prompt
;;; whenever necessary.

(define-module (ossau interactive prompting)
  #:use-module (ice-9 buffered-input)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 readline)
  #:export (port-prompt-setter
	    set-port-prompts!
	    prompt-string
	    prompt-if-not-char-ready?
	    make-prompting-input-port))

;; port-prompt-setter PORT
;;
;; A prompt setter is a property of an input port.  It is a procedure
;; with signature (<procedure> PROMPT [CONTINUATION-PROMPT]) that
;; stores off the specified prompts for later use as and when the port
;; needs to issue a prompt for more input.  PROMPT and
;; CONTINUATION-PROMPT should be strings or thunks that evaluate to a
;; string.  If CONTINUATION-PROMPT is not specified or #f, PROMPT is
;; used for both new and continuing input.

(define port-prompt-setter (make-object-property))

;; set-port-prompts! PORT PROMPT [CONTINUATION-PROMPT]
;;
;; Convenience wrapper to set the prompts of a port that has a prompt
;; setter property.

(define (set-port-prompts! port prompt . continuation-prompt)
  (apply (port-prompt-setter port) prompt continuation-prompt)
  (set-buffered-input-continuation?! port #f))

;; prompt-string PROMPT CONTINUATION-PROMPT CONTINUATION?
;;
;; Given the prompt args PROMPT and CONTINUATION-PROMPT passed to a
;; port-prompt-setter, and a flag CONTINUATION? indicating whether a
;; prompt is required for continuation (as opposed to new) input,
;; return the prompt string to use.

(define (prompt-string prompt continuation-prompt continuation?)
  (or (and continuation?
	   continuation-prompt
	   (if (string? continuation-prompt)
	       continuation-prompt
	       (continuation-prompt)))
      (and prompt
	   (if (string? prompt)
	       prompt
	       (prompt)))
      ""))

;; prompt-if-not-char-ready? INPUT-PORT OUTPUT-PORT PROMPT
;;                           CONTINUATION-PROMPT CONTINUATION?
;;
;; Utility procedure for a character-stream INPUT-PORT that prompts by
;; displaying a prompt string to the specified OUTPUT-PORT.  If there
;; is no input available on INPUT-PORT, this procedure calculates the
;; appropriate prompt string using the PROMPT, CONTINUATION-PROMPT and
;; CONTINUATION? args, and displays it.

(define (prompt-if-not-char-ready? input-port
				   output-port
				   prompt
				   continuation-prompt
				   continuation?)
  (or (char-ready? input-port)
      (begin
	(if (defined? 'usleep)
	    (usleep 100000)
	    (sleep 1))
	(char-ready? input-port))
      (begin
	(display (prompt-string prompt continuation-prompt continuation?)
		 output-port)
	(force-output output-port))))

;; make-prompting-input-port INPUT-PORT OUTPUT-PORT TRANSLATOR

(define (make-prompting-input-port input-port output-port translator)
  (let ((input-port-prompt-setter (port-prompt-setter input-port)))
    (if input-port-prompt-setter
	(if translator
	    (let ((port (make-line-buffered-input-port
			 (lambda (continuation?)
			   (set-buffered-input-continuation?! input-port
							      continuation?)
			   (translator (read-line input-port))))))
	      (set! (port-prompt-setter port) input-port-prompt-setter)
	      port)
	    input-port)
	(letrec ((prompt #f)
		 (continuation-prompt #f)
		 (port (make-line-buffered-input-port
			(lambda (continuation?)
			  (prompt-if-not-char-ready? input-port
						     output-port
						     prompt
						     continuation-prompt
						     continuation?)
			  ((or translator identity) (read-line input-port))))))
	  (set! (port-prompt-setter port)
		(lambda (p . cp)
		  (set! prompt p)
		  (set! continuation-prompt
			(if (null? cp)
			    #f
			    (car cp)))))
	  port))))



;;; {Readline Integration}
;;;
;;; The readline port handles its own prompting.  Here we integrate
;;; the readline port with this formalism by giving it a
;;; port-prompt-setter that interfaces to (ice-9 readline)'s interface
;;; for setting the readline prompts.

(set! (port-prompt-setter (readline-port)) set-readline-prompt!)
