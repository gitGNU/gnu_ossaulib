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

(define-module (ossau db file scm-alist)
  #:use-module (ossau db generics)
  #:use-module (ossau filesys)
  #:use-module (oop goops)
  #:export (<db-file-scm-alist>
	    db-keys))

(define-class <db-file-scm-alist> ()
  (f #:init-value #f  #:accessor db-file-name)
  (p #:init-value #f  #:accessor db-port)
  (m #:init-value #f  #:accessor db-modified?)
  (c #:init-value '() #:accessor db-cache))

(define-method (db-open (db <db-file-scm-alist>) file-name)
  (if (db-file-name db)
      (error "Database already open for file:" (db-file-name db)))
  (set! (db-port db)
        (if (file-exists? file-name)
            (open-input-file file-name)
            #f))
  (set! (db-file-name db) file-name))

(define-method (db-close (db <db-file-scm-alist>))
  (if (db-file-name db)
      (begin
        (if (db-port db)
            (begin
              (close-input-port (db-port db))
              (set! (db-port db) #f)))
        (if (db-modified? db)
            (begin
              (mkpath (db-file-name db))
              (let ((p (open-output-file (db-file-name db))))
                (with-output-to-port p
                  (lambda ()
                    (map (lambda (entry)
                           (write entry)
                           (newline))
                         (db-cache db))))
                (close-output-port p))
              (set! (db-modified? db) #f)))
        (set! (db-cache db) '())
        (set! (db-file-name db) #f))))

(define-method (db-sync (db <db-file-scm-alist>))
  (if (not (db-file-name db))
      (error "Database is not open!"))
  (let ((file-name (db-file-name db)))
    (db-close db)
    (db-open db file-name)))

(define-method (db-read-next (db <db-file-scm-alist>))
  (let ((p (db-port db)))
    (if p
        (let ((next-entry (with-input-from-port p read)))
          (cond
           ((eof-object? next-entry) #f)
           ((pair? next-entry)
            (set! (db-cache db)
                  (append (db-cache db) (list next-entry)))
            next-entry)
           (else (error "Corrupt database entry!"))))
        #f)))

(define-method (db-keys (db <db-file-scm-alist>))
  (if (not (db-file-name db))
      (error "Database is not open!"))
  (let loop ((keys (map car (db-cache db)))
             (next-entry (db-read-next db)))
    (if (not next-entry)
        keys
        (loop (append keys (list (car next-entry)))
              (db-read-next db)))))

(define-method (db-ref (db <db-file-scm-alist>) key)
  (if (not (db-file-name db))
      (error "Database is not open!"))
  (or (assoc-ref (db-cache db) key)
      (let loop ((next-entry (db-read-next db)))
        (if (not next-entry)
            #f
            (if (equal? (car next-entry) key)
                (cdr next-entry)
                (loop (db-read-next db)))))))

(define-method (db-set! (db <db-file-scm-alist>) key value)
  (if (not (db-file-name db))
      (error "Database is not open!"))
  (db-keys db)
  (set! (db-cache db)
        (assoc-set! (db-cache db) key value))
  (set! (db-modified? db) #t))
