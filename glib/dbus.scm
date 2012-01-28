
(define-module (glib dbus)
  #:use-module (system foreign)
  #:use-module (glib variant)
  #:use-module (glib object)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ossau trc)
  #:export (dbus-interface
	    dbus-call
	    dbus-connect))

(define gio (dynamic-link "libgio-2.0"))

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
  (trc 'dbus-call interface method)
  (let ((parms (scheme->variant parameters))
	(errloc (uint-list->bytevector '(0)
				       (native-endianness)
				       (sizeof '*))))
    (trc 'dbus-call-parms (variant->string parms) parms)
    (let* ((result (g_dbus_proxy_call_sync interface
					   (string->pointer method)
					   parms
					   0
					   1000
					   %null-pointer
					   (bytevector->pointer errloc)))
	   (gerror (make-pointer (car (bytevector->uint-list errloc
							     (native-endianness)
							     (sizeof '*))))))
      (trc 'dbus-call-result (variant->string result) result)
      (trc 'dbus-call-error gerror)
      (or (null-pointer? gerror)
	  (let ((parsed-error (parse-c-struct gerror (list int32 int '*))))
	    (trc 'dbus-error-code (cadr parsed-error))
	    (trc 'dbus-error-msg (pointer->string (caddr parsed-error)))))
      (variant->scheme result))))

(define interface-signal-alist (make-object-property))

(define (dbus-connect interface dbus-signal proc)
  (let ((signal-alist (interface-signal-alist interface)))
    (or signal-alist
	(gobject-connect interface
			 "g-signal"
			 (lambda (signal parameters)
			   (trc 'dbus-signal signal parameters)
			   (let ((handler
				  (assoc-ref (interface-signal-alist interface)
					     signal)))
			     (if handler
				 (apply handler parameters))))))
    (set! (interface-signal-alist interface)
	  (assoc-set! (or signal-alist '())
		      dbus-signal
		      proc))))
