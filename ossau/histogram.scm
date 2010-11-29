;;;;
;;;; 	Copyright (C) 2010 Neil Jerram.
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

(define-module (ossau histogram)
  #:export (make-histogram-counter))

(define (make-histogram-counter min max inc)
  ;; Map a value to the histogram bin that it is counted in.
  (define (bin value)
    (inexact->exact (floor (/ (- value min) inc))))
  ;; Work out how many bins we need, and allocate a vector of that
  ;; size.
  (let* ((num-bins (1+ (bin max)))
	 (histogram (make-vector num-bins 0)))
    ;; Return a lambda that will count a given value (by adding 1 to
    ;; the appropriate bin); and which will return the current
    ;; histogram if called with argument #f.
    (lambda (value)
      (if value
	  (let ((bin-index (bin value)))
	    (if (or (< bin-index 0)
		    (>= bin-index num-bins))
		(format (current-error-port)
			"Warning: value ~a is outside histogram range\n"
			value)
		(vector-set! histogram
			     bin-index
			     (1+ (vector-ref histogram bin-index)))))
	  histogram))))
