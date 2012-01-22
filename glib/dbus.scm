
(define-module (glib dbus)
  #:use-module (system foreign)
  #:export (g_dbus_proxy_new_for_bus_sync
	    G_BUS_TYPE_SYSTEM
	    G_BUS_TYPE_SESSION
	    G_DBUS_PROXY_FLAGS_NONE
	    g_dbus_proxy_call_sync))

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
