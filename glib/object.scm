
(define-module (glib object)
  #:use-module (system foreign)
  #:use-module (glib variant)
  #:use-module (ossau trc)
  #:export (gobject-unref
	    gobject-connect
	    gobject-disconnect))

(define gobject (dynamic-link "libgobject-2.0"))

(define g_signal_connect_data
  (pointer->procedure unsigned-long
		      (dynamic-func "g_signal_connect_data" gobject)
		      (list '*		; object
			    '*		; detailed signal
			    '*		; GCallback
			    '*		; closure data
			    '*		; destroy data
			    int		; connect flags
			    )))

(define gobject-unref
  (pointer->procedure void
		      (dynamic-func "g_object_unref" gobject)
		      (list '*		; object
			    )))

(define gobject-signal-handlers (make-object-property))

(define (gobject-connect object signal proc)
  (trc 'gobject-connect object signal proc)
  (let* ((handler (lambda (proxy sender signal parameters)
		    (trc 'proxy proxy)
		    (trc 'sender sender)
		    (proc (pointer->string signal)
			  (variant->scheme parameters))))
	 (ptr (procedure->pointer void
				  handler
				  (list '* '* '* '*))))
    ;; Protect this handler from being collected.
    (set! (gobject-signal-handlers object)
	  (acons signal
		 (cons handler ptr)
		 (or (gobject-signal-handlers object)
		     '())))
    ;; Register the handler.
    (g_signal_connect_data object
			   (string->pointer signal)
			   ptr
			   %null-pointer
			   %null-pointer
			   0)))

(define gobject-disconnect
  (pointer->procedure void
		      (dynamic-func "g_signal_handler_disconnect" gobject)
		      (list '*		; object
			    unsigned-long ; id
			    )))
