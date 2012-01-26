
(define-module (glib dbus)
  #:use-module (system foreign)
  #:use-module (glib variant)
  #:export (dbus-interface
	    dbus-call))

;; (define glib (dynamic-link "libglib-2.0"))

(define gio (dynamic-link "libgio-2.0"))

;; (define g_main_loop_new
;;   (pointer->procedure '*
;; 		      (dynamic-func "g_main_loop_new" glib)
;; 		      (list '* int)))
;; 
;; (define loop (g_main_loop_new %null-pointer FALSE))
;; 
;; (write loop)
;; (newline)

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

(define (dbus-interface bus-type service object-path interface-name)
  (g_dbus_proxy_new_for_bus_sync (assq-ref `((system .  ,G_BUS_TYPE_SYSTEM)
					     (session . ,G_BUS_TYPE_SESSION))
					   bus-type)
				 G_DBUS_PROXY_FLAGS_NONE
				 %null-pointer
				 (string->pointer service)
				 (string->pointer object-path)
				 (string->pointer interface-name)
				 %null-pointer
				 %null-pointer))

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

(define (dbus-call interface method . parameters)
  (variant->scheme (g_dbus_proxy_call_sync interface
					   (string->pointer method)
					   (scheme->variant parameters)
					   0
					   1000
					   %null-pointer
					   %null-pointer)))
