
(define-module (ossau trc)
  #:use-module (ice-9 format)
  #:export-syntax (trc))

(define program-name (basename (car (command-line))))

(define-syntax-rule (trc stuff ...)
  (trc* (current-source-location) stuff ...))

(define (trc* loc . args)
  (let ((preamble (format #f
			  "~8x ~12a ~5@a: "
			  (object-address (current-thread))
			  (basename (or (assq-ref loc 'filename) ""))
			  (assq-ref loc 'line))))
    (display preamble)
    ;; When args is just a single string, display it; otherwise write
    ;; the whole arg list.
    (if (and (pair? args)
	     (null? (cdr args))
	     (string? (car args)))
	(display (car args))
	(write args))
    (newline)))
