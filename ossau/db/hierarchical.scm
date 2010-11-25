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

(define-module (ossau db hierarchical)
  #:use-module (ossau db generics)
  #:use-module (ossau db file scm-alist)
  #:use-module (oop goops)
  #:export (<db-hierarchical>))

(define-class <db-hierarchical> ()

  ;; Root name of open database.  This will prefix the keys that are
  ;; used to open leaf databases, and so should be meaningful to the
  ;; leaf database class.
  (r #:init-value #f
     #:accessor db-root-name)

  ;; Leaf name for leaf databases.  This will suffix the keys that are
  ;; used to open leaf databases, and so should be meaningful to the
  ;; leaf database class.
  (m #:init-value "/db.scm"
     #:accessor db-leaf-name
     #:init-keyword #:leaf-name)

  ;; Database class used to implement the actual storage.
  (c #:init-value <db-file-scm-alist>
     #:accessor db-leaf-class
     #:init-keyword #:leaf-class)

  ;; Key hierarchical delimiter.
  (d #:init-value #\/
     #:accessor db-hierarchy-delimiter
     #:init-keyword #:hierarchy-delimiter)

  ;; The number of key components that will be used to form the key in the leaf database.
  (n #:init-value 1
     #:accessor db-leaf-depth
     #:init-keyword #:leaf-depth)

  ;; Cache of open leaf databases - an alist mapping keys to leaf database instances.
  (l #:init-value '()
     #:accessor db-leaves)

  ;; Maximum number of writes between syncs.
  (t #:init-value 50
     #:accessor db-sync-threshold)

  ;; Number of writes since last sync.
  (u #:init-value 0
     #:accessor db-writes-since-last-sync))

(define-method (db-open (db <db-hierarchical>) root-name)
  (if (db-root-name db)
      (error "Database already open for root name:"
             (db-root-name db)))
  (set! (db-root-name db) root-name))

(define-method (db-close (db <db-hierarchical>))
  (if (db-root-name db)
      (begin
        (map (lambda (leaf-entry)
               (db-close (cdr leaf-entry)))
             (db-leaves db))
        (set! (db-leaves db) '()))))

(define-method (db-sync (db <db-hierarchical>))
  (if (not (db-root-name db))
      (error "Database is not open!"))
  (map (lambda (leaf-entry)
         (db-sync (cdr leaf-entry)))
       (db-leaves db)))

(define-method (db-leaf (db <db-hierarchical>) key)
  (let ((delim (db-hierarchy-delimiter db)))

    ;; Check that the key starts with the hierarchy delimiter.
    (or (eq? (string-ref key 0) delim)
        (error "Database key does not start with hierarchy delimiter!"))

    ;; Find the nth slash from last, where n is the database leaf depth.
    (let loop ((hd-pos (string-rindex key delim))
               (depth (- (db-leaf-depth db) 1)))

      (if (and hd-pos (> depth 0))
          (loop (string-rindex key delim #f hd-pos))

          ;; Now the key can be divided into leaf database name and
          ;; leaf database key at hd-pos.
          (begin
            (or hd-pos (set! hd-pos 0))

            (let* ((leaf-db-name (string-append
                                 (db-root-name db)
                                 (substring key 0 hd-pos)
                                 (db-leaf-name db)))
                   (leaf-db-key (substring key (+ hd-pos 1)))
                   (leaf-db (assoc-ref (db-leaves db) leaf-db-name)))

              (if (not leaf-db)
                  (begin
                    (set! leaf-db (make (db-leaf-class db)))
                    (db-open leaf-db leaf-db-name)
                    (set! (db-leaves db)
                          (assoc-set! (db-leaves db)
                                      leaf-db-name
                                      leaf-db))))

              (cons leaf-db leaf-db-key)))))))

(define-method (db-ref (db <db-hierarchical>) key)
  (if (not (db-root-name db))
      (error "Database is not open!"))
  (let ((leaf (db-leaf db key)))
    (db-ref (car leaf) (cdr leaf))))

(define-method (db-set! (db <db-hierarchical>) key value)
  (if (not (db-root-name db))
      (error "Database is not open!"))
  (let ((leaf (db-leaf db key)))
    (db-set! (car leaf) (cdr leaf) value))
  (if (< (db-writes-since-last-sync db)
         (db-sync-threshold db))
      (set! (db-writes-since-last-sync db)
            (1+ (db-writes-since-last-sync db)))
      (begin
        (db-sync db)
        (set! (db-writes-since-last-sync db) 0))))
