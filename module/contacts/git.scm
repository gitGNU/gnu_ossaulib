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

(define-module (contacts git)
  #:use-module (contacts common)
  #:use-module (oop goops)
  #:use-module (utils)
  #:export (import->git
	    git->export))

;; Import contacts from their native format into a branch of the Git repository.
(define (import->git class source branch)

  ;; Initialize the Git repository if it doesn't already exist.
  (ensure-repository)

  (with-working-directory repository-dir

    ;; Import and `git add' master format contact files.
    (import-and-stage-contacts class source branch)

    ;; Commit them as an import.
    (system/format "git commit -m ~s" (format #f
					      "Import ~a ~a"
					      (class-name class)
					      source))
    
    ;; Switch to master and cherry-pick the changes.
    (system/format "git checkout master")
    (system/format "git cherry-pick ~a" branch)))

(define (import-and-stage-contacts class source branch)

  ;; Try to switch to the named branch.
  (or (zero? (system/format "git checkout ~a >/dev/null 2>&1" branch))
      ;; Branch does not already exist, so must be created.
      (begin
	(system/format "git checkout ~a" root-tag-name)
	(system/format "git checkout -b ~a" branch)))

  ;; Delete all existing contacts.
  (system/format "git rm -f _*")

  ;; Read contacts from the source.
  (import->dir class source)

  ;; Stage the generated contacts.
  (system/format "git add _*"))

(define contacts-dir (in-vicinity (getenv "HOME") ".contacts"))
(define repository-dir (in-vicinity contacts-dir "repository"))
(define repository-stamp-file-name "CONTACTS")
(define root-tag-name "___root___")

(define (ensure-repository)
  (ensure-directory contacts-dir)
  (ensure-directory repository-dir)
  (with-working-directory repository-dir
    (or (file-exists? ".git")
	(begin
	  (system/format "git init")
	  (with-output-to-file repository-stamp-file-name
	    (lambda ()
	      (format #t "Created on ~a\n"
		      (strftime "%Y%m%d-%H%M" (localtime (current-time))))))
	  (system/format "git add ~s" repository-stamp-file-name)
	  (system/format "git commit -m ~s" "Repository created")
	  (system/format "git tag ~a" root-tag-name)))))

(define (git->export branch class target)

  ;; Initialize the Git repository if it doesn't already exist.
  (ensure-repository)

  (with-working-directory repository-dir

    ;; Switch to the master branch.
    (system/format "git checkout master")

    ;; If the branch that we're exporting to doesn't already exist,
    ;; create it here.
    (system/format "git branch ~a" branch)

    ;; Export to the target.
    (dir->export class target)

    ;; Now reimport from the target.  The point of this is that the
    ;; target format probably doesn't preserve every field that we
    ;; have in the master format, and we don't want a future import of
    ;; this target to look like a _deletion_ of those non-preserved
    ;; fields.
    (import-and-stage-contacts class target branch)

    ;; Commit them as an export and reimport.
    (system/format "git commit -m ~s" "Export and reimport")

    ;; Switch back to master.
    (system/format "git checkout master")))
