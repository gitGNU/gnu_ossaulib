
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
	    (phone-numbers (aref bbdb-entry 4))
	    (other-bbdb-info (vector (aref bbdb-entry 2) ;AKAs
				     (aref bbdb-entry 3) ;???
				     (aref bbdb-entry 5) ;addresses
				     (aref bbdb-entry 6) ;emails
				     (aref bbdb-entry 7) ;notes
				     (aref bbdb-entry 8) ;???
				     )))
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
	    (insert "X-OTHER-BBDB-INFO\n"
		    " ")
	    (prin1 other-bbdb-info (current-buffer))
	    (insert "\n")
	    (write-region (point-min) (point-max) contact-file))))
      (setq bbdb-entry (condition-case nil
			   (read (current-buffer))
			 (end-of-file nil))))))
