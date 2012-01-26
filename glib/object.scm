
(define-module (glib object)
  #:use-module (system foreign)
  #:use-module (glib variant)
  #:export (gobject-connect))

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

(define (gobject-connect object signal proc)
  (g_signal_connect_data
   object
   (string->pointer signal)
   (procedure->pointer void
		       (lambda (proxy sender signal parameters user_data)
			 (proc (pointer->string signal)
			       (variant->scheme parameters)))
		       (list '* '* '* '* '*))
   %null-pointer
   %null-pointer
   0))
