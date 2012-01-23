
(define-module (e17 edje)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (run-edje))

(define eina (dynamic-link "libeina"))
(define evas (dynamic-link "libevas"))
(define ecore (dynamic-link "libecore"))
(define ecore_evas (dynamic-link "libecore_evas"))
(define edje (dynamic-link "libedje"))

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

(define (run-edje edj-file)
  (dynamic-call "eina_init" eina)
  (dynamic-call "evas_init" evas)
  (dynamic-call "ecore_init" ecore)
  (dynamic-call "ecore_evas_init" ecore_evas)
  (dynamic-call "edje_init" edje)

  (let* ((window (ecore_evas_software_x11_new %null-pointer 0 0 0 480 580))
	 (canvas (ecore_evas_get window))
	 (edje (create-my-group canvas edj-file)))
    (write window)
    (newline)
    (ecore_evas_show window)
    (dynamic-call "ecore_main_loop_begin" ecore)
    (evas_object_del edje)
    (ecore_evas_free window)))

(define (create-my-group canvas edj-file)
  (let ((edje (edje_object_add canvas)))
    (write (edje_object_file_set edje
				 (string->pointer edj-file)
				 (string->pointer "my_group")))
    (newline)
    (evas_object_move edje 0 0)
    (evas_object_resize edje 480 580)
    (evas_object_show edje)  
    edje))
