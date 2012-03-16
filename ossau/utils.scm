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

(define-module (ossau utils)
  #:export (ensure-directory
	    system/format
	    repeat-until-no-exception)
  #:export-syntax (with-working-directory))

(cond-expand (guile-2)
	     (else (use-syntax (ice-9 syncase))))

(define-syntax with-working-directory
  (syntax-rules ()
    ((with-working-directory dir body ...)
     (let ((outer (getcwd)))
       (dynamic-wind
	 (lambda () (chdir dir))
	 (lambda () body ...)
	 (lambda () (chdir outer)))))))

(define (ensure-directory dir)
  (if (file-exists? dir)
      (or (file-is-directory? dir)
	  (error "`~a' already exists but is not a directory." dir))
      (mkdir dir)))

(define (with-dir-and-base-name file proc)
  (with-working-directory (dirname file)
    (proc (basename file))))

(define (system/format string . args)
  (system (apply format #f string args)))

(define (repeat-until-no-exception thunk sleep-interval)
  (car (let loop ()
	 (or (false-if-exception
	      (begin
		(list (thunk))))
	     (begin
	       (sleep sleep-interval)
	       (loop))))))
