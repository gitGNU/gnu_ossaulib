
(defvar bbdb-file (expand-file-name "~/BBDB"))

(defun bbdb-export (dir)
  (interactive "DExport to directory: ")
  (let ((outdir (file-name-as-directory
		 (expand-file-name 
		  (format-time-string "%Y%m%d-%H%M")
		  dir)))
	(bbdb-entry))
    (make-directory outdir)
    (set-buffer (find-file-noselect bbdb-file))
    (goto-char (point-min))
    (setq bbdb-entry (read (current-buffer)))
    (while bbdb-entry
      (let ((first-names (aref bbdb-entry 0))
	    (last-name (aref bbdb-entry 1))
	    (x-field-2 (aref bbdb-entry 2)) ;AKAs
	    (x-field-3 (aref bbdb-entry 3)) ;???
	    (phone-numbers (aref bbdb-entry 4))
	    (x-field-5 (aref bbdb-entry 5)) ;addresses
	    (x-field-6 (aref bbdb-entry 6)) ;emails
	    (x-field-7 (aref bbdb-entry 7)) ;notes
	    (x-field-8 (aref bbdb-entry 8)) ;???
	    )
	(let* ((contact-name (concat "_" first-names "_" last-name))
	       (contact-file (concat outdir contact-name)))
	  (save-excursion
	    (set-buffer (get-buffer-create "*BBDB-Export*"))
	    (erase-buffer)
	    (insert "FIRST-NAMES\n"
		    " " (or first-names "") "\n"
		    "LAST-NAME\n"
		    " " (or last-name "") "\n")
	    (while phone-numbers
	      (let ((phone (car phone-numbers)))
		(insert "PHONE " (aref phone 0) "\n"
			" " (aref phone 1) "\n"))
	      (setq phone-numbers (cdr phone-numbers)))
	    (let ((x-fields (list (cons 2 x-field-2)
				  (cons 3 x-field-3)
				  (cons 5 x-field-5)
				  (cons 6 x-field-6)
				  (cons 7 x-field-7)
				  (cons 8 x-field-8)))
		  x-field)
	      (while x-fields
		(setq x-field (car x-fields)
		      x-fields (cdr x-fields))
		(insert "X-BBDB-FIELD-" (number-to-string (car x-field)) "\n"
			" ")
		(prin1 (cdr x-field) (current-buffer))
		(insert "\n")))
	    (write-region (point-min) (point-max) contact-file))))
      (setq bbdb-entry (condition-case nil
			   (read (current-buffer))
			 (end-of-file nil))))))
