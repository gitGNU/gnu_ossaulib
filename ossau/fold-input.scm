;;;; (ossau fold-input)
;;;; 
;;;; 	Copyright (C) 2009 Free Software Foundation, Inc.
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free
;;;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;;; Boston, MA 02110-1301 USA

(define-module (ossau fold-input)
  #:export (fold-input
	    grep-fold))

;; ---------------------------------------------------------------------
;; fold-input ACC-PROC INIT INPUT-PROC
;;
;; Call INPUT-PROC repeatedly until it returns EOF, and use ACC-PROC
;; to fold up the results, starting with INIT.
;; ---------------------------------------------------------------------
(define (fold-input acc-proc init input-proc)

  ;; Loop, with the accumulator value starting as INIT.
  (let loop ((acc init))

    ;; Call INPUT-PROC.
    (let ((next-input (input-proc)))

      ;; If it returned EOF, return the current accumulator value.
      (if (eof-object? next-input)
	  acc

	  ;; Otherwise call ACC-PROC to form a new accumulator value,
	  ;; and loop with that.
	  (loop (acc-proc next-input acc))))))

;; ---------------------------------------------------------------------
;; grep-fold ACC-PROC INIT REGEXP . SUBSTITUTE-ITEMS
;;
;; Fold together the set of lines from the current input port that
;; match REGEXP, by first processing each matching line according to
;; SUBSTITUTE-ITEMS, then accumulating them using ACC-PROC, with INIT
;; as the initial accumulator value.
;; ---------------------------------------------------------------------
(define (grep-fold acc-proc init regexp . substitute-items)

  (define (regexp-exec-and-substitute rx line . substitute-items)
    (and-let* ((match (regexp-exec rx line)))
      (if (null? substitute-items)
	  line
	  (apply regexp-substitute #f match substitute-items))))

  (let ((rx (make-regexp regexp)))
    (fold-input (lambda (line acc)
		  (or (and-let* ((sub (apply regexp-exec-and-substitute
					     rx line substitute-items)))
			(acc-proc sub acc))
		      acc))
		'()
		read-line)))
