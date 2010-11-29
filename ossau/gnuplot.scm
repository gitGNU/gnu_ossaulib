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

(define-module (ossau gnuplot)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 popen)
  #:export (gnuplot))

(define* (gnuplot file-name plot-command #:key key)
  (let ((pipe (open-output-pipe "gnuplot -")))
    (if (string? file-name)
	(format pipe
		"\
set terminal png
set output '~a'
~a
~a
quit
"
		file-name
		(if key "" "unset key")
		plot-command)
	(format pipe
		"\
~a
~a
"
		(if key "" "unset key")
		plot-command))
    (if (string? file-name)
	(close-pipe pipe))))

;; Example:
;; (gnuplot "table1.png"
;;    "plot 'table1.txt' using 1:6 with lines, 4*x/log(2*x), 4*x/log(x)")
