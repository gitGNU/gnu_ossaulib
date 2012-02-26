
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

(define-module (ossau trc)
  #:use-module (ice-9 format)
  #:export-syntax (trc))

(define program-name (basename (car (command-line))))

(define-syntax-rule (trc stuff ...)
  (trc* (current-source-location) stuff ...))

(define (trc* loc . args)
  (let ((preamble (format #f
			  "~a ~12a ~5@a: "
			  (strftime "%b %d %T" (localtime (current-time)))
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
    (newline))
  (force-output))

