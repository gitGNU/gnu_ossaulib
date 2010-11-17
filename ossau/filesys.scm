;;;; filesys.scm --- miscellaneous filesystem-related functions
;;;;
;;;;    Copyright (C) 1999 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 
;;;; Contributed by Neil Jerram <mpriz@dircon.co.uk>



(define-module (ossau filesys)
  #:export (mkpath
	    directory-select
	    directory-subdirectories
	    directory-files
	    directory-subtree
	    expand-file-name))



;;; {Miscellaneous Filesystem Functions}

(define file-is-directory?
  (if (provided? 'i/o-extensions)
      (lambda (str)
	(eq? (stat:type (lstat str)) 'directory))
      (lambda (str)
	(display str)
	(newline)
	(let ((port (catch 'system-error
			   (lambda () (open-file (string-append str "/.")
						 OPEN_READ))
			   (lambda args #f))))
	  (if port (begin (close-port port) #t)
	      #f)))))

;; mkpath PATH
;;
;; Create the sequence of directories required for the pathname PATH.
;; The component of PATH after the last forward slash is interpreted
;; as a file name, not as a directory path component.  This makes it
;; convenient to call mkpath with the full file name of a file that
;; you are about to create, as a way of ensuring that the containing
;; directories exist.
(define (mkpath path)
  (let* ((slash (string-rindex path #\/))
         (dir (and slash
                   (> slash 0)
                   (substring path 0 slash))))
    (cond
     ((not dir) ;; Nothing to do.
      )
     ((and (file-exists? dir)
           (not (file-is-directory? dir)))
      (error "Path includes an existing file name!"))
     ((file-exists? dir) ;; Nothing to do.
      )
     (else
      (mkpath dir)
      (mkdir dir)))))

;; remove-if-tr PREDICATE LIST
;;
;; Tail recursive version of remove-if.
(define remove-if-tr
  (lambda (p l)
    (and (list? l)
         (letrec ((-remove (lambda (checked remaining)
                             (cond
                              ((null? remaining)
                               checked)
                              ((p (car remaining))
                               (-remove checked (cdr remaining)))
                              (else
                               (-remove (append checked (list (car remaining)))
                                        (cdr remaining)))))))
           (-remove '() l)))))

;; reverse-directory-listing DIR
;;
;; Returns a list of file names in directory DIR.  The ordering is reversed
;; with respect to the ordering produced naturally by readdir.
(define (reverse-directory-listing dir-name)
  (let ((dir (opendir dir-name)))
    (let loop ((dir-list '())
               (dir-entry (readdir dir)))
      (cond ((eof-object? dir-entry)
             dir-list)
            ((or (string=? dir-entry ".")
                 (string=? dir-entry ".."))
             (loop dir-list (readdir dir)))
            (else
             (loop (cons (string-append dir-name "/" dir-entry)
                         dir-list)
                   (readdir dir)))))))

;; directory-select DIR PREDICATE
;;
;; Returns a list of files in directory DIR that satisfy PREDICATE.
;; PREDICATE takes a single argument - the fully qualified name of the
;; file to test.  The ordering is the same as the ordering produced
;; naturally by readdir.
(define (directory-select dir-name selector)
  (let loop ((selected '())
             (dir-list (reverse-directory-listing dir-name)))
    (cond
     ((null? dir-list)
      selected)
     ((selector (car dir-list))
      (loop (cons (car dir-list) selected) (cdr dir-list)))
     (else
      (loop selected (cdr dir-list))))))

;; directory-subdirectories DIR
;;
;; Returns a list of subdirectories of directory DIR.  The ordering is
;; the same as the ordering produced naturally by readdir.
(define (directory-subdirectories dir-name)
  (directory-select dir-name file-is-directory?))

;; directory-files DIR
;;
;; Returns a list of non-directory files in directory DIR.  The
;; ordering is the same as the ordering produced naturally by readdir.
(define (directory-files dir-name)
  (directory-select dir-name (lambda (f) (not (file-is-directory? f)))))

;; directory-subtree DIR
;;
;; Returns a list of non-directory files in the subtree rooted at DIR.
(define (directory-subtree dir-name)
  (append (directory-files dir-name)
          (apply append
                 (map directory-subtree (directory-subdirectories dir-name)))))

;; expand-file-name FILE-NAME
;;
;; Canonicalize and expand the specified file name.
(define (expand-file-name file-name)
  (let ((home (getenv "HOME")))
    (if (and home
             (string=? "~/" (substring file-name 0 2)))
        (set! file-name
              (in-vicinity home (substring file-name 2)))))
  file-name)
