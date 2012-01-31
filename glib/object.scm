
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
