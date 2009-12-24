
(require 'cl)

(defvar bbdb-entries nil)
(defvar bbdb-entry nil)
(defvar next-entry (read t))

(defun bbdb-name-field (entry sym)
  (caddr (assq sym entry)))

(defun bbdb-x-field (entry sym)
  (read (caddr (assq sym entry))))

(defun bbdb-phone-fields (entry)
  (let (phones entry-item)
    (while entry
      (setq entry-item (car entry)
	    entry (cdr entry))
      (if (eq (car entry-item) 'PHONE)
	  (setq phones
		(cons (vector (cadr entry-item) (caddr entry-item))
		      phones))))
    (nreverse phones)))

(while next-entry
  (setq bbdb-entry (vector (bbdb-name-field next-entry 'FIRST-NAMES)
			   (if (assq 'X-BBDB-LAST-NAME-NIL next-entry)
			       nil
			     (bbdb-name-field next-entry 'LAST-NAME))
			   (bbdb-x-field next-entry 'X-BBDB-FIELD-2)
			   (bbdb-x-field next-entry 'X-BBDB-FIELD-3)
			   (bbdb-phone-fields next-entry)
			   (bbdb-x-field next-entry 'X-BBDB-FIELD-5)
			   (bbdb-x-field next-entry 'X-BBDB-FIELD-6)
			   (bbdb-x-field next-entry 'X-BBDB-FIELD-7)
			   (bbdb-x-field next-entry 'X-BBDB-FIELD-8)))
  (setq bbdb-entries (cons bbdb-entry bbdb-entries)
	next-entry (condition-case nil
		       (read t)
		     (error nil))))

(defun bbdb-sort (e1 e2)
  ;; Sort algorithm copied from `bbdb-record-sortkey'.
  (string-lessp (downcase (concat (aref e1 1) (aref e1 0)))
		(downcase (concat (aref e2 1) (aref e2 0)))))
 
(setq bbdb-entries (sort bbdb-entries (function bbdb-sort)))

(let ((print-escape-newlines t))
  (while bbdb-entries
    (prin1 (car bbdb-entries))
    (terpri)
    (setq bbdb-entries (cdr bbdb-entries))))
