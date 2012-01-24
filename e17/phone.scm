
(define-module (e17 phone)
  #:use-module (e17 edje)
  #:export (create-show-phone-ui
	    enable-buttons
	    disable-buttons
	    show-current-call
	    clear-current-call
	    run-ui-loop))

;; Callback to call when a button is pressed.  The callback will be
;; called as (button-pressed GROUP NAME), where GROUP is a symbol
;; specifying a group of buttons that can be enabled or disabled
;; together: 'dialer, 'call, 'hangup or 'speaker; and NAME is the name
;; (a string) of the specific button that was pressed.
(define button-pressed #f)

;; Edje object for the UI.
(define edje #f)

;; Create and show the phone UI.
(define (create-show-phone-ui button-pressed-cb)
  (set! button-pressed button-pressed-cb)
  (set! edje (edje-create-and-show "phone.edj" 480 580))
  (edje-connect edje
		"mouse,down,*"
		"one"
		(lambda _
		  (button-pressed 'dialer "one"))))

;; Run the UI's main loop.
(define (run-ui-loop)
  (edje-main-loop)
  (edje-cleanup edje))

;; Enable a set of buttons.  GROUP is a symbol specifying a group of
;; buttons that can be enabled or disabled together: 'dialer, 'call,
;; 'hangup or 'speaker.
(define (enable-buttons group)
  (case group
    ((dialer call hangup speaker)
     (edje-emit edje (format #f "~a,enable" group) ""))
    (else (error "No such group:" group))))


;; Disable a set of buttons.  GROUP is a symbol specifying a group of
;; buttons that can be enabled or disabled together: 'dialer, 'call,
;; 'hangup or 'speaker.
(define (disable-buttons group)
  (case group
    ((dialer call hangup speaker)
     (edje-emit edje (format #f "~a,disable" group) ""))
    (else (error "No such group:" group))))

;; Show information about the current incoming or outgoing call.
;; NUMBER is a number to display.  INCOMING-RINGING indicates that the
;; call is an incoming one that the local user has not yet answered or
;; rejected, and hence a ringtone should be played.  CONTACT is a
;; (currently abstract) object holding information about the contact
;; that the call is with; it could include a name and/or image to
;; display, and a ringtone to play instead of the default ringtone
;; when INCOMING-RINGING is #t.
;;
;; If the UI is already displaying some other current call
;; information, the new information replaces it.
(define (show-current-call number incoming-ringing contact)
  ...)

;; Clear any displayed "current call" information.
(define (clear-current-call)
  ...)
