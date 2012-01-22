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

(cond-expand (guile-2
	      (add-to-load-path
	       (dirname
		(dirname
		 (canonicalize-path (current-filename))))))
	     (else
	      (let* ((bindir (dirname (car (command-line))))
		     (absdir (cond ((string=? bindir ".")
				    (getcwd))
				   ((string-match "^/" bindir)
				    bindir)
				   (else
				    (in-vicinity (getcwd) bindir)))))
		(set! %load-path (cons (in-vicinity absdir "..")
				       %load-path)))))
