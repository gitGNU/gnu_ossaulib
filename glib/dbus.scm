
(define-module (glib dbus)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (g_dbus_proxy_new_for_bus_sync
	    G_BUS_TYPE_SYSTEM
	    G_BUS_TYPE_SESSION
	    G_DBUS_PROXY_FLAGS_NONE
	    g_dbus_proxy_call_sync
	    g_variant_print
	    g_variant_get_type
	    print-variant
	    g_variant_get_child_value
	    g_variant_get_string
	    g_variant_new_string
	    g_variant_new_boolean
	    g_variant_new_tuple
	    FALSE
	    TRUE))

(define gobject (dynamic-link "libgobject-2.0"))
(define glib (dynamic-link "libglib-2.0"))
(define gio (dynamic-link "libgio-2.0"))

(write gobject)
(newline)
(write glib)
(newline)
(write gio)
(newline)

(dynamic-call "g_type_init" gobject)

(define FALSE 0)
(define TRUE 1)

(define g_main_loop_new
  (pointer->procedure '*
		      (dynamic-func "g_main_loop_new" glib)
		      (list '* int)))

(define loop (g_main_loop_new %null-pointer FALSE))

(write loop)
(newline)

(define g_dbus_proxy_new_for_bus_sync
  (pointer->procedure '*
		      (dynamic-func "g_dbus_proxy_new_for_bus_sync" gio)
		      (list int		; bus type
			    int		; flags
			    '*		; interface info
			    '*		; bus name
			    '* 		; object path
			    '*		; interface name
			    '*		; cancellable
			    '*		; error
			    )))


;; bus type
(define G_BUS_TYPE_SYSTEM 1)
(define G_BUS_TYPE_SESSION 2)

;; flags
(define G_DBUS_PROXY_FLAGS_NONE 0)

(define g_dbus_proxy_call_sync
  (pointer->procedure '*
		      (dynamic-func "g_dbus_proxy_call_sync" gio)
		      (list '*		; proxy
			    '*		; method_name
			    '*		; parameters
			    int		; flags
			    int		; timeout_msec
			    '*		; cancellable
			    '*		; error
			    )))

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
