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

(define-module (contacts csv)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 string-fun)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:export (read-csv))

(define (read-csv file-name)
  (let ((s (utf16->string (get-bytevector-all (open-input-file file-name))
			  'little)))

    ;; Discard possible byte order mark.
    (if (and (>= (string-length s) 1)
	     (char=? (string-ref s 0) #\xfeff))
	(set! s (substring s 1)))

    ;; Split out the header line, which tells us how many fields there
    ;; should be in each following line.
    (split-discarding-char #\newline s
      (lambda (header-line rest)
	(let* ((headers (separate-fields-discarding-char #\, header-line list))
	       (nheaders (length headers)))

	  ;; Loop reading data fields.
	  (let loop ((data '())
		     (entry '())
		     (headers-to-read headers)
		     (rest rest))

	    (cond ((null? headers-to-read)
		   (loop (cons (reverse! entry) data)
			 '()
			 headers
			 rest))

		  ((zero? (string-length rest))
		   (reverse! data))

		  ((char=? (string-ref rest 0) #\")
		   (receive (value after)
		       (with-input-from-string rest
			 (lambda ()
			   (values (read)
				   (begin
				     (read-char)
				     (let loop ((chars '())
						(next (read-char)))
				       (if (eof-object? next)
					   (list->string (reverse! chars))
					   (loop (cons next chars)
						 (read-char))))))))
		     (loop data
			   (acons (car headers-to-read) value entry)
			   (cdr headers-to-read)
			   after)))

		  (else
		   (split-discarding-char (if (null? (cdr headers-to-read))
					      #\newline
					      #\,)
					  rest
		     (lambda (value rest)
		       (loop data
			     (acons (car headers-to-read) value entry)
			     (cdr headers-to-read)
			     rest)))))))))))
