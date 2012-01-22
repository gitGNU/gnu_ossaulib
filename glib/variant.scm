
(define-module (glib variant)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (g_variant_print
	    g_variant_get_type
	    print-variant
	    g_variant_get_child_value
	    g_variant_get_string
	    g_variant_new_string
	    g_variant_new_boolean
	    g_variant_new_tuple
	    FALSE
	    TRUE))

(define FALSE 0)
(define TRUE 1)

(define gobject (dynamic-link "libgobject-2.0"))
(define glib (dynamic-link "libglib-2.0"))

(dynamic-call "g_type_init" gobject)

(define g_variant_get_child_value
  (pointer->procedure '*
		      (dynamic-func "g_variant_get_child_value" glib)
		      (list '*		; variant
			    int		; index
			    )))

(define g_variant_print
  (pointer->procedure '*
		      (dynamic-func "g_variant_print" glib)
		      (list '*		; variant
			    int		; type annotate
			    )))

(define g_variant_get_type
  (pointer->procedure '*
		      (dynamic-func "g_variant_get_type" glib)
		      (list '*		; variant
			    )))

(define g_variant_get_string
  (pointer->procedure '*
		      (dynamic-func "g_variant_get_string" glib)
		      (list '*		; variant
			    '*		; length
			    )))

(define (print-variant variant)
  (if (null-pointer? variant)
      (display "(null variant pointer)")
      (begin
	(display (pointer->string (g_variant_get_type variant)))
	(display ": ")
	(display (pointer->string (g_variant_print variant FALSE)))))
  (newline))

(define g_variant_new_string
  (pointer->procedure '*
		      (dynamic-func "g_variant_new_string" glib)
		      (list '*		; string
			    )))

(define g_variant_new_boolean
  (pointer->procedure '*
		      (dynamic-func "g_variant_new_boolean" glib)
		      (list int		; boolean
			    )))

(define g_variant_new_tuple
  (pointer->procedure '*
		      (dynamic-func "g_variant_new_tuple" glib)
		      (list '*		; GVariant **
			    int		; num children
			    )))
