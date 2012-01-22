#! /usr/bin/guile -s
!#

(cond-expand (guile-2 (include "setup-load-path.scm"))
             (else (load "setup-load-path.scm")))

(use-modules (glib dbus)
	     (glib variant)
	     (system foreign)
	     (rnrs bytevectors))

(define manager-proxy
  (g_dbus_proxy_new_for_bus_sync G_BUS_TYPE_SYSTEM
				 G_DBUS_PROXY_FLAGS_NONE
				 %null-pointer
				 (string->pointer "org.ofono")
				 (string->pointer "/")
				 (string->pointer "org.ofono.Manager")
				 %null-pointer
				 %null-pointer))
(write manager-proxy)
(newline)

(define return-parms (g_dbus_proxy_call_sync manager-proxy
					     (string->pointer "GetModems")
					     %null-pointer
					     0
					     1000
					     %null-pointer
					     %null-pointer))
(print-variant return-parms)

(define modems (g_variant_get_child_value return-parms 0))
(print-variant modems)

(define first-modem (g_variant_get_child_value modems 0))
(print-variant first-modem)

(define modem-name (g_variant_get_child_value first-modem 0))
(print-variant modem-name)

(define modem-name-string
  (pointer->string (g_variant_get_string modem-name %null-pointer)))
(format #t "First modem's name is ~a\n" modem-name-string)

(define modem-proxy
  (g_dbus_proxy_new_for_bus_sync G_BUS_TYPE_SYSTEM
				 G_DBUS_PROXY_FLAGS_NONE
				 %null-pointer
				 (string->pointer "org.ofono")
				 (string->pointer modem-name-string)
				 (string->pointer "org.ofono.Modem")
				 %null-pointer
				 %null-pointer))

(write modem-proxy)
(newline)

(define powered (g_variant_new_string (string->pointer "Powered")))
(print-variant powered)
(define true (g_variant_new_boolean TRUE))
(print-variant true)

(define bv (uint-list->bytevector (map pointer-address
				       (list powered true))
				  (native-endianness)
				  (sizeof '*)))
(write bv)
(newline)

(define parms (g_variant_new_tuple (bytevector->pointer bv) 2))
(print-variant parms)

(define return-parms
  (g_dbus_proxy_call_sync modem-proxy
			  (string->pointer "SetProperty")
			  parms
			  0
			  1000
			  %null-pointer
			  %null-pointer))

(print-variant return-parms)
