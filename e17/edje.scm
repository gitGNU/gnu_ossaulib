
(define-module (e17 edje)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ossau trc)
  #:export (edje-create-and-show
	    edje-main-loop
	    edje-connect
	    edje-emit
	    edje-cleanup
	    edje-text-set))

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

;; void(* Edje_Signal_Cb)(void *data, Evas_Object *obj, const char *emission, const char *source)
(define (Edje_Signal_Cb proc)
  (procedure->pointer void
		      (lambda (data obj emission source)
			(proc obj
			      (pointer->string emission)
			      (pointer->string source)))
		      (list '* '* '* '*)))

;; Register a CALLBACK to be called when the part named SOURCE, in
;; Edje object EDJE, emits the specified SIGNAL.
(define (edje-connect edje signal source callback)
  (edje_object_signal_callback_add edje
				   (string->pointer signal)
				   (string->pointer source)
				   (Edje_Signal_Cb callback)
				   %null-pointer))

;; Emit the specified SIGNAL, with source SOURCE, in Edje object EDJE.
(define (edje-emit edje signal source)
  (edje_object_signal_emit edje
			   (string->pointer signal)
			   (string->pointer source)))

(define edje-window (make-object-property))

(define (edje-create-and-show edj-file width height)
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
