
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

(define-module (glib dbus)
  #:use-module (system foreign)
  #:use-module (glib variant)
  #:use-module (glib object)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ossau trc)
  #:use-module (oop goops)
  #:export (dbus-interface
	    dbus-call
	    dbus-call-async
	    dbus-connect
	    dbus-interface-release))

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

(define (dbus-interface-release interface)
  (trc 'dbus-interface-release interface)
  (let ((id (interface-signal-id interface)))
    (trc 'id id)
    (if id
	(gobject-disconnect interface id)))
  (set! (interface-signal-id interface) #f)
  (set! (interface-signal-alist interface) #f)
  (gobject-unref interface))

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

(define g_dbus_proxy_call
  (pointer->procedure void
		      (dynamic-func "g_dbus_proxy_call" gio)
		      (list '*		; proxy
			    '*		; method_name
			    '*		; parameters
			    int		; flags
			    int		; timeout_msec
			    '*		; cancellable
			    '*		; callback
			    '*		; user_data
			    )))

(define g_dbus_proxy_call_finish
  (pointer->procedure '*
		      (dynamic-func "g_dbus_proxy_call_finish" gio)
		      (list '*		; proxy
			    '*		; res
			    '*		; error
			    )))

(define (dbus-call interface method . parameters)
  (trc 'dbus-call interface method parameters)
  (let ((timeout 10000))
    (if (and (pair? parameters)
	     (eq? (car parameters) #:timeout))
	(begin
	  (set! timeout (cadr parameters))
	  (set! parameters (cddr parameters))))
    (let ((parms (scheme->variant parameters))
	  (errloc (uint-list->bytevector '(0)
					 (native-endianness)
					 (sizeof '*))))
      (trc 'dbus-call-parms (variant->string parms) parms)
      (let* ((result (g_dbus_proxy_call_sync interface
					     (string->pointer method)
					     parms
					     0
					     timeout
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
	      (trc 'dbus-error-msg (pointer->string (caddr parsed-error)))
	      (throw 'dbus-error
		     (cadr parsed-error)
		     (pointer->string (caddr parsed-error)))))
	(variant->scheme result)))))

(define interface-callbacks (make-object-property))

(define (dbus-call-async completion-proc interface method . parameters)
  (trc 'dbus-call-async interface method parameters)
  (let* ((timeout 10000)
	 (callback (lambda (source_object res user_data)
		     (let* ((errloc (uint-list->bytevector '(0)
							   (native-endianness)
							   (sizeof '*)))
			    (result (g_dbus_proxy_call_finish interface
							      res
							      (bytevector->pointer errloc)))
			    (gerror (make-pointer (car (bytevector->uint-list errloc
									      (native-endianness)
									      (sizeof '*))))))
		       (trc 'dbus-call-async-result (variant->string result) result)
		       (trc 'dbus-call-async-error gerror)
		       (completion-proc (if (null-pointer? gerror)
					    (variant->scheme result)
					    #f)
					(if (null-pointer? gerror)
					    #f
					    (let ((parsed-error (parse-c-struct gerror (list int32 int '*))))
					      (trc 'dbus-call-async-error-code (cadr parsed-error))
					      (trc 'dbus-call-async-error-msg (pointer->string (caddr parsed-error)))
					      (cons (cadr parsed-error)
						    (pointer->string (caddr parsed-error)))))))))
	 (ptr (procedure->pointer void
				  callback
				  (list '* '* '*))))
    (set! (interface-callbacks interface)
	  (acons callback
		 ptr
		 (or (interface-callbacks interface)
		     '())))
    (if (and (pair? parameters)
	     (eq? (car parameters) #:timeout))
	(begin
	  (set! timeout (cadr parameters))
	  (set! parameters (cddr parameters))))
    (let ((parms (scheme->variant parameters)))
      (trc 'dbus-call-async-parms (variant->string parms) parms)
      (g_dbus_proxy_call interface
			 (string->pointer method)
			 parms
			 0
			 timeout
			 %null-pointer
			 ptr
			 %null-pointer))))

(define interface-signal-id (make-object-property))
(define interface-signal-alist (make-object-property))

(define (dbus-connect interface dbus-signal proc)
  (trc 'dbus-connect interface dbus-signal proc)
  (or (interface-signal-id interface)
      (let* ((g-signal-handler (lambda (signal parameters)
				 (trc 'dbus-signal interface signal parameters)
				 (let ((handler
					(assoc-ref (interface-signal-alist interface)
						   signal)))
				   (if handler
				       (apply handler parameters)))))
	     (id (gobject-connect interface
				  "g-signal"
				  g-signal-handler)))
	(trc "Set up g-signal handler" 'id id)
	(set! (interface-signal-id interface) id)))
  (set! (interface-signal-alist interface)
	(assoc-set! (or (interface-signal-alist interface) '())
		    dbus-signal
		    proc)))
