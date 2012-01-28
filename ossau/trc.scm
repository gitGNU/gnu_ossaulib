
(define-module (ossau trc)
  #:use-module (ice-9 format)
  #:export-syntax (trc))

(define program-name (basename (car (command-line))))

(define-syntax-rule (trc stuff ...)
  (trc* (current-source-location) stuff ...))

(define (trc* loc . args)
  (let ((preamble (format #f
			  "~12a ~5@a: "
			  (basename (or (assq-ref loc 'filename) ""))
			  (assq-ref loc 'line))))
    (display preamble)
    (write args)
    (newline)))
