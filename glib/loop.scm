
;;;;
;;;; 	Copyright (C) 2013 Neil Jerram.
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

(define-module (glib loop)
  #:use-module (system foreign)
  #:use-module (glib variant)
  #:use-module (ossau trc)
  #:export (loop-run
	    loop-add-fd))

(define glib (dynamic-link "libglib-2.0"))

(define g_main_loop_new
  (pointer->procedure '*		; GMainLoop *
		      (dynamic-func "g_main_loop_new" glib)
		      (list '*		; GMainContext *, or NULL
			    int		; is_running
			    )))

(define g_main_loop_run
  (pointer->procedure void
		      (dynamic-func "g_main_loop_run" glib)
		      (list '*		; GMainLoop *
			    )))

(define *main-loop* (g_main_loop_new %null-pointer 0))

(define (loop-run)
  (g_main_loop_run *main-loop*))
