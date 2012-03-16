
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

(define-module (ofono registration)
  #:use-module (glib dbus)
  #:use-module (ossau trc)
  #:use-module (ofono modem)
  #:export (add-registration-hook))

;; Changes to modem registration state are indicated by calling this
;; hook with args STATE and PROPERTIES.  STATE can be 'none, meaning
;; that there is currently no modem; 'unregistered, meaning that there
;; is a modem but it isn't registered with the network; or
;; 'registered, meaning that the modem is registered with the network.
;; If STATE is 'registered, PROPERTIES is an alist of registration
;; properties; otherwise PROPERTIES is #f.
(define registration-hook (make-hook 2))

(define (add-registration-hook proc)
  (add-hook! registration-hook proc))

(define (notify-registration state properties)
  (run-hook registration-hook state properties))

(define modem-state-hook
  (let ((reg-interface #f))
    (lambda (path properties)

      (define (registration-changed)
	(let ((props (car (dbus-call reg-interface "GetProperties"))))
	  (trc 'reg-properties props)
	  (notify-registration 'registered props)))
      
      (if properties
	  ;; Modem exists.
	  (let ((interfaces (assoc-ref properties "Interfaces")))
	    (if (member "org.ofono.NetworkRegistration" interfaces)
		;; Modem is registered.
		(if (not reg-interface)
		    ;; We haven't already connected to the
		    ;; registration interface.
		    (begin
		      (set! reg-interface
			    (dbus-interface 'system
					    "org.ofono"
					    path
					    "org.ofono.NetworkRegistration"))
		      (dbus-connect reg-interface
				    "PropertyChanged"
				    (lambda _ (registration-changed)))
		      (registration-changed)
		      (dbus-call reg-interface "Register")))
		;; Modem is not registered.
		(if reg-interface
		    (begin
		      (set! reg-interface #f)
		      (notify-registration 'unregistered #f)))))
	  ;; Modem has disappeared.
	  (begin
	    (set! reg-interface #f)
	    (notify-registration 'none #f))))))

(add-modem-state-hook modem-state-hook)
