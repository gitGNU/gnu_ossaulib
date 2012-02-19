
;;;;
;;;; 	Copyright (C) 2012 Neil Jerram.
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

(define-module (e17 edje)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ossau trc)
  #:export (edje-create-and-show
	    edje-main-loop
	    edje-connect
	    edje-emit
	    edje-cleanup
	    edje-text-set
	    edje-part-state))

(define eina (dynamic-link "libeina"))
(define evas (dynamic-link "libevas"))
(define ecore (dynamic-link "libecore"))
(define ecore_evas (dynamic-link "libecore_evas"))
(define edje (dynamic-link "libedje"))

(define ecore_main_loop_glib_integrate
  (pointer->procedure int8
		      (dynamic-func "ecore_main_loop_glib_integrate" ecore)
		      '()))

(define ecore_evas_new
  (pointer->procedure '*
		      (dynamic-func "ecore_evas_new" ecore_evas)
		      (list '*
			    int
			    int
			    int
			    int
			    '*
			    )))

(define ecore_evas_software_x11_new
  (pointer->procedure '*
		      (dynamic-func "ecore_evas_software_x11_new" ecore_evas)
		      (list '*
			    int
			    int
			    int
			    int
			    int
			    )))

(define ecore_evas_get
  (pointer->procedure '*
		      (dynamic-func "ecore_evas_get" ecore_evas)
		      (list '*
			    )))

(define ecore_evas_show
  (pointer->procedure void
		      (dynamic-func "ecore_evas_show" ecore_evas)
		      (list '*
			    )))

(define evas_object_del
  (pointer->procedure void
		      (dynamic-func "evas_object_del" evas)
		      (list '*
			    )))

(define evas_object_move
  (pointer->procedure void
		      (dynamic-func "evas_object_move" evas)
		      (list '* int int)))

(define evas_object_resize
  (pointer->procedure void
		      (dynamic-func "evas_object_resize" evas)
		      (list '* int int)))

(define evas_object_show
  (pointer->procedure void
		      (dynamic-func "evas_object_show" evas)
		      (list '*)))

(define ecore_evas_free
  (pointer->procedure void
		      (dynamic-func "ecore_evas_free" ecore_evas)
		      (list '*
			    )))

(define edje_object_add
  (pointer->procedure '*
		      (dynamic-func "edje_object_add" edje)
		      (list '*
			    )))

(define edje_object_file_set
  (pointer->procedure int
		      (dynamic-func "edje_object_file_set" edje)
		      (list '*
			    '*
			    '*
			    )))

(define edje_object_signal_callback_add
  (pointer->procedure void
		      (dynamic-func "edje_object_signal_callback_add" edje)
		      (list '*		; Evas_Object
			    '*		; signal name
			    '*		; source name
			    '*		; Edje_Signal_Cb func
			    '*		; closure data
			    )))


(define edje_object_signal_emit
  (pointer->procedure void
		      (dynamic-func "edje_object_signal_emit" edje)
		      (list '*		; Evas_Object
			    '*		; signal name
			    '*		; source name
			    )))

(define edje_object_part_text_set
  (pointer->procedure int8
		      (dynamic-func "edje_object_part_text_set" edje)
		      (list '*		; Evas_Object
			    '*		; part name
			    '*		; text name
			    )))

(define (edje-text-set edje part text)
  (edje_object_part_text_set edje
			     (string->pointer part)
			     (string->pointer text)))

(define edje-signal-handlers (make-object-property))

;; Register a CALLBACK to be called when the part named SOURCE, in
;; Edje object EDJE, emits the specified SIGNAL.
(define (edje-connect edje signal source callback)
  (let* ((handler (lambda (data obj emission source)
		    (callback obj
			      (pointer->string emission)
			      (pointer->string source))))
	 (ptr (procedure->pointer void
				  handler
				  (list '* '* '* '*))))
    ;; Protect this handler from being collected.
    (set! (edje-signal-handlers edje)
	  (acons (cons signal source)
		 (cons handler ptr)
		 (or (edje-signal-handlers edje) '())))
    ;; Register the handler.
    (edje_object_signal_callback_add edje
				     (string->pointer signal)
				     (string->pointer source)
				     ptr
				     %null-pointer)))

;; Emit the specified SIGNAL, with source SOURCE, in Edje object EDJE.
(define (edje-emit edje signal source)
  (edje_object_signal_emit edje
			   (string->pointer signal)
			   (string->pointer source)))

(define edje_object_part_state_get
  (pointer->procedure '*
		      (dynamic-func "edje_object_part_state_get" edje)
		      (list '*		; Evas_Object
			    '*		; part name
			    '*		; double pointer
			    )))

(define (edje-part-state edje part)
  (pointer->string (edje_object_part_state_get edje
					       (string->pointer part)
					       %null-pointer)))

(define ecore_evas_title_set
  (pointer->procedure '*
		      (dynamic-func "ecore_evas_title_set" ecore_evas)
		      (list '*		; Ecore_Evas
			    '*		; title
			    )))

(define edje-window (make-object-property))

(define (edje-create-and-show edj-file width height title)
  (dynamic-call "eina_init" eina)
  (dynamic-call "evas_init" evas)
  (dynamic-call "ecore_init" ecore)
  (dynamic-call "ecore_evas_init" ecore_evas)
  (dynamic-call "edje_init" edje)

  (let* ((window (ecore_evas_software_x11_new
		  %null-pointer 0
		  0 0
		  width height))
	 (canvas (ecore_evas_get window))
	 (edje (create-my-group canvas edj-file width height)))
    (ecore_evas_title_set window (string->pointer title))
    (ecore_evas_show window)
    (set! (edje-window edje) window)
    edje))

(define (edje-main-loop)
  (trc 'ecore-glib-integration  (ecore_main_loop_glib_integrate))
  (dynamic-call "ecore_main_loop_begin" ecore))

(define (edje-cleanup edje)
  (let ((window (edje-window edje)))
    (evas_object_del edje)
    (ecore_evas_free window)))

(define (create-my-group canvas edj-file width height)
  (let ((edje (edje_object_add canvas)))
    (trc 'edje_object_file_set
	 'rc
	 (edje_object_file_set edje
			       (string->pointer edj-file)
			       (string->pointer "my_group")))
    (evas_object_move edje 0 0)
    (evas_object_resize edje width height)
    (evas_object_show edje)  
    edje))
