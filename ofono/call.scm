
(define-module (ofono call)
  #:use-module (glib dbus)
  #:export (dial
	    set-incoming-call-proc))

;; This module maps from oFono's D-Bus API for voice call handling to
;; a higher-level and Schemey Scheme API.  As a shorthand we refer to
;; the code using that Scheme API as the UI (for User Interface).

;; (dial number answered failed) - Initiate an outgoing call.
;;
;; NUMBER is the number to dial.
;;
;; ANSWERED is a procedure that (ofono call) will call, if the call is
;; answered, as (ANSWERED make-active-call).  MAKE-ACTIVE-CALL is a
;; procedure that the UI should then call to construct an object that
;; represents the active call, specifying its callbacks for events
;; that may occur on that call: (MAKE-ACTIVE-CALL call-gone
;; dtmf-digit-received).
;;
;;   CALL-GONE should be a thunk that (ofono call) will call if the
;;   active call is terminated by the network or by the other end.
;;
;;   DTMF-DIGIT-RECEIVED should be a procedure that (ofono call) will
;;   call, as (DTMF-DIGIT-RECEIVED digit), if the other end sends a
;;   DTMF digit.
;;
;;   (MAKE-ACTIVE-CALL ...) returns two values: a thunk that the UI
;;   can call when it wants to hang up the call: (hang-up); and a
;;   procedure that the UI can call to send a DTMF digit:
;;   (send-dtmf-digit digit).
;;
;; FAILED should be a procedure that (ofono call) will call if the
;; outgoing call attempt fails, as (FAILED reason).
;;
;; (dial ...) returns a thunk that the module user can invoke in order
;; to cancel the outgoing call attempt.  This is only valid if done
;; before either ANSWERED or FAILED is called.
(define (dial number answered failed)
  (let* ((voice-call-manager (get-voice-call-manager))
	 (call (dbus-call voice-call-manager "Dial" number "default"
			  (lambda (error)
			    (failed (error->reason error)))))
	 (call-gone #f)
	 (dtmf-digit-received #f))
    (dbus-connect call
		  "PropertyChanged"
		  (lambda (property value)
		    (cond ((eq? property 'State)
			   (case value
			     ((active)
			      (answered (make-make-active-call call
							       (lambda (arg)
								 (set! call-gone arg))
							       noop)))
			     ((disconnected)
			      (if call-gone
				  ;; Call was active, so use call-gone.
				  (call-gone)
				  ;; Call was still dialing.
				  (failed value))))))))
    (lambda ()
      (dbus-call call "Hangup"))))

(define (make-make-active-call call store-call-gone store-dtmf-digit-received)
  (lambda (call-gone dtmf-digit-received)
    (store-call-gone call-gone)
    (store-dtmf-digit-received dtmf-digit-received)
    (values (lambda ()
	      (dbus-call call "Hangup"))
	    (lambda (digit)
	      (dbus-call call "SendDTMF" digit)))))

;; (set-incoming-call-proc incoming-call) - Register a procedure to be
;; called if there is an incoming call.
;;
;; When there is an incoming call, (ofono call) will call
;; INCOMING-CALL as (INCOMING-CALL make-active-call reject).
;; MAKE-ACTIVE-CALL is a procedure with the same purpose and signature
;; as described above under 'dial'.  REJECT is a thunk that the UI can
;; call to reject the incoming call.  The INCOMING-CALL invocation
;; should return a thunk that (ofono call) can call to indicate that
;; the incoming call is no longer available.
;;
;; The anticipated mainline scenario is that the UI indicates to its
;; human user that there is an incoming call (e.g. by playing a ring
;; tone), and that the user indicates, after some interval, if they
;; want to take or reject the call.  During that interval it is
;; possible for the call to disappear (e.g. because the other end
;; stops waiting and hangs up) and this should be indicated to the
;; human user too.
;;
;; To support that scenario, the UI's incoming-call procedure should
;; save off the MAKE-ACTIVE-CALL and REJECT procedures, then
;; immediately return a call-gone thunk.  This allows (ofono call) to
;; call that thunk, if the call disappears, while the user is still
;; deciding whether to answer the call.
;;
;; An alternate scenario - e.g. when a call is handled automatically
;; according to a predefined set of rules - is where the UI knows
;; immediately whether it wants to accept or reject the incoming call.
;; In that case it can call MAKE-ACTIVE-CALL or REJECT from inside the
;; incoming-call procedure, and doesn't need to return a call-gone
;; thunk.
(define (set-incoming-call-proc incoming-call)
  (let ((voice-call-manager (get-voice-call-manager)))
    (dbus-connect voice-call-manager
		  "CallAdded"
		  (lambda (call properties)
		    (if (eq? (assq-ref properties 'State) 'incoming)
			(let* ((active-call-gone #f)
			       (rejected #f)
			       (unanswered-call-gone
				(incoming-call (make-make-active-call call
								      (lambda (arg)
									(set! active-call-gone arg))
								      noop)
					       (lambda ()
						 (dbus-call call "Hangup")
						 (set! rejected #t)))))
			  (or rejected
			      (dbus-connect call
					    "PropertyChanged"
					    ...))))))))

(define get-voice-call-manager
  (let ((already-created #f))
    (lambda ()
      (or already-created
	  (let* ((manager-interface (dbus-interface 'system
						    "org.ofono"
						    "/"
						    "org.ofono.Manager"))
		 (manager-parms (dbus-call manager-interface
					   "GetModems"))
		 (modem-name (caaar manager-parms))
		 (modem-interface (dbus-interface 'system
						  "org.ofono"
						  modem-name
						  "org.ofono.Modem"))
		 (vcm-interface (dbus-interface 'system
						"org.ofono"
						modem-name
						"org.ofono.VoiceCallManager")))
	    (format #t "Modem name is ~a\n" modem-name)
	    (dbus-call modem-interface "SetProperty" "Powered" #t)
	    (set! already-created vcm-interface)
	    vcm-interface)))))
