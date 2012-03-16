
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

(define-module (ofono modem)
  #:use-module (glib dbus)
  #:use-module (ossau trc)
  #:use-module (ossau utils)
  #:export (add-modem-state-hook
	    connect-to-ofono))

;; Changes to modem state or properties are indicated by calling this
;; hook with args PATH and PROPERTIES.  If PROPERTIES is #f, it means
;; that the modem has just disappeared; otherwise PROPERTIES is an
;; alist.
(define modem-state-hook (make-hook 2))

(define (add-modem-state-hook proc)
  (add-hook! modem-state-hook proc))

(define (notify-modem-state path properties)
  (run-hook modem-state-hook path properties))

(define (connect-to-ofono)

  ;; Use oFono's Manager interface.
  (let ((manager-interface (dbus-interface 'system
					   "org.ofono"
					   "/"
					   "org.ofono.Manager"))
	(hso-modem-path #f))

    ;; (maybe-use-modem path): If PATH begins with "/hso", and we
    ;; haven't already got a current HSO modem, call hso-modem-added.
    (define (maybe-use-modem path)
      (trc 'maybe-use-modem path)
      (or hso-modem-path
	  (not (string-prefix? "/hso" path))
	  (begin
	    (set! hso-modem-path path)
	    (hso-modem-added))))

    ;; Start using the modem whose path is in HSO-MODEM-PATH.
    (define (hso-modem-added)
      (trc 'hso-modem-added hso-modem-path)
      (let* ((modem-interface (dbus-interface 'system
					      "org.ofono"
					      hso-modem-path
					      "org.ofono.Modem")))

	;; Define handler for modem property changes.
	(define (property-changed)
	  ;; Get current properties.
	  (let ((props (car (dbus-call modem-interface
				       "GetProperties"))))
	    (trc 'props props)
	    (notify-modem-state hso-modem-path props)
	    ;; If the modem is now powered but not online, try to set
	    ;; it online.  Note that we could be racing with other
	    ;; modem users here, so allow for the possibility of error
	    ;; (specifically "Operation already in progress").
	    (if (and (assoc-ref props "Powered")
		     (not (assoc-ref props "Online")))
		(catch 'dbus-error
		  (lambda ()
		    (dbus-call modem-interface "SetProperty" "Online" #t))
		  (lambda _
		    (trc "Failed to set modem online - probably racing with another modem user"))))))
    
	;; Immediate code
	(trc 'modem-interface modem-interface)

	;; Register handler for modem property changes.
	(dbus-connect modem-interface
		      "PropertyChanged"
		      (lambda _ (property-changed)))

	;; Also call property-changed right now.
	(property-changed)

	;; Power on the modem.  Note that we could be racing with
	;; other modem users here, so allow for the possibility of
	;; error (specifically "Operation already in progress").
	(catch 'dbus-error
	  (lambda ()
	    (dbus-call modem-interface "SetProperty" "Powered" #t))
	  (lambda _
	    (trc "Failed to power the modem on - probably racing with another modem user")))))

    ;; Register handler for ModemAdded signal: Call maybe-use-modem.
    (dbus-connect manager-interface
		  "ModemAdded"
		  (lambda (path properties)
		    (trc 'modem-added path properties)
		    (maybe-use-modem path)))

    ;; Register handler for ModemRemoved signal: If the modem being
    ;; removed is hso-modem-path, call the hso-modem-removed-proc.
    (dbus-connect manager-interface
		  "ModemRemoved"
		  (lambda (path)
		    (trc 'modem-removed path)
		    (if (string=? path hso-modem-path)
			(begin
			  (set! hso-modem-path #f)
			  (notify-modem-state hso-modem-path #f)))))

    ;; Start of day processing: Call GetModems, and do maybe-use-modem
    ;; for each modem returned.
    (for-each (lambda (modem)
		(maybe-use-modem (car modem)))
	      ;; oFono may not have started up yet, so we might need
	      ;; to call GetModems more than once before it succeeds.
	      (car (repeat-until-no-exception
		    (lambda ()
		      (dbus-call manager-interface "GetModems"))
		    10)))))
