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

(define-module (ossau contacts google)
  #:use-module (ossau contacts common)
  #:use-module (ice-9 regex)
  #:use-module (oop goops)
  #:use-module (ossau contacts csv)
  #:export (<google>))

(define-class <google> (<contacts>))

(define (google-key->native key)
  (string-append "X-GOOGLE-"
		 (regexp-substitute/global #f " " key 'pre "_" 'post)))

;; Read a Google-exported CSV file.
(define-method (native-read (db <google>) csv-file)

  ;; Read the CSV file.
  (let ((data (read-csv csv-file)))
    
    ;; Convert spaces in the keys to underscores, and prefix them with
    ;; "X-GOOGLE-".
    (map (lambda (alist)
	   (map (lambda (entry)
		  (set-car! entry (google-key->native (car entry)))
		  entry)
		alist))
	 data)))

;; Given a Google record, compute standard contact fields from it.
(define-method (compute-standard-fields (db <google>) record)
  `(("FIRST-NAMES" . ,(let ((given (assoc-ref record "X-GOOGLE-Given_Name"))
			    (additional (assoc-ref record "X-GOOGLE-Additional_Name")))
			(cond ((and given additional)
			       (string-append given " " additional))
			      (given given)
			      (additional additional)
			      (else ""))))
    ,@(google->native-phone-fields record)
    ,@(google->native-email-fields record)
    ,@(map-named-fields record
			'(("LAST-NAME" . "X-GOOGLE-Family_Name")
			  ("NOTES" . "X-GOOGLE-Notes")))))

    
(define keys '("Name"
	       "Given Name"
	       "Additional Name"
	       "Family Name"
	       "Yomi Name"
	       "Given Name Yomi"
	       "Additional Name Yomi"
	       "Family Name Yomi"
	       "Name Prefix"
	       "Name Suffix"
	       "Initials"
	       "Nickname"
	       "Short Name"
	       "Maiden Name"
	       "Birthday"
	       "Gender"
	       "Location"
	       "Billing Information"
	       "Directory Server"
	       "Mileage"
	       "Occupation"
	       "Hobby"
	       "Sensitivity"
	       "Priority"
	       "Subject"
	       "Notes"
	       "Group Membership"
	       "E-mail 1 - Type"
	       "E-mail 1 - Value"
	       "E-mail 2 - Type"
	       "E-mail 2 - Value"
	       "E-mail 3 - Type"
	       "E-mail 3 - Value"
	       "Phone 1 - Type"
	       "Phone 1 - Value"
	       "Phone 2 - Type"
	       "Phone 2 - Value"
	       "Phone 3 - Type"
	       "Phone 3 - Value"
	       "Phone 4 - Type"
	       "Phone 4 - Value"
	       "Address 1 - Type"
	       "Address 1 - Formatted"
	       "Address 1 - Street"
	       "Address 1 - City"
	       "Address 1 - PO Box"
	       "Address 1 - Region"
	       "Address 1 - Postal Code"
	       "Address 1 - Country"
	       "Address 1 - Extended Address"
	       "Address 2 - Type"
	       "Address 2 - Formatted"
	       "Address 2 - Street"
	       "Address 2 - City"
	       "Address 2 - PO Box"
	       "Address 2 - Region"
	       "Address 2 - Postal Code"
	       "Address 2 - Country"
	       "Address 2 - Extended Address"
	       "Organization 1 - Type"
	       "Organization 1 - Name"
	       "Organization 1 - Yomi Name"
	       "Organization 1 - Title"
	       "Organization 1 - Department"
	       "Organization 1 - Symbol"
	       "Organization 1 - Location"
	       "Organization 1 - Job Description"))

;; Given a set of standard contact fields, compute corresponding
;; native Google fields.
(define-method (compute-native-fields (db <google>) record)
  `(("X-GOOGLE-Name" . ,(let ((first (assoc-ref record "FIRST-NAMES"))
			      (last (assoc-ref record "LAST-NAME")))
			  (cond ((and first last)
				 (string-append first
						" "
						last))
				((or first last) => identity) 
				(else ""))))
    ,@(native->google-email-fields record)
    ,@(native->google-phone-fields record)
    ,@(map-named-fields record
			'(("X-GOOGLE-Given_Name" . "FIRST-NAMES")
			  ("X-GOOGLE-Family_Name" . "LAST-NAME")
			  ("X-GOOGLE-Notes" . "NOTES")))))

(define (field-map record indices combiner . key-formats)
  (apply map
	 combiner
	 (map (lambda (key-format)
		(map (lambda (key)
		       (assoc-ref record key))
		     (map google-key->native (map (lambda (index)
						    (format #f
							    key-format
							    index))
						  indices))))
	      key-formats)))

(define (google->native-email-fields record)
  (let ((emails (filter! (lambda (entry)
			   (and (car entry) (cdr entry)))
			 (field-map record
				    '(1 2 3)
				    cons
				    "E-mail ~a - Type"
				    "E-mail ~a - Value"))))
    (if (null? emails)
	'()
	`(("EMAIL" ,@emails)))))

(define (google->native-phone-fields record)
  (let ((phones (filter! (lambda (entry)
			   (and (car entry) (cdr entry)))
			 (field-map record
				    '(1 2 3 4)
				    cons
				    "Phone ~a - Type"
				    "Phone ~a - Value"))))
    (if (null? phones)
	'()
	`(("PHONE" ,@phones)))))

(define (native->google-email-fields record)
  (let ((emails (assoc-ref record "EMAIL")))
    (let loop ((emails (or emails '()))
	       (index 1)
	       (google-fields '()))
      (if (or (null? emails)
	      (> index 3))
	  google-fields
	  (loop (cdr emails)
		(1+ index)
		(acons (google-key->native (format #f "E-mail ~a - Type" index))
		       (caar emails)
		       (acons (google-key->native (format #f
							  "E-mail ~a - Value"
							  index))
			      (cdar emails)
			      google-fields)))))))

(define (native->google-phone-fields record)
  (let ((phones (assoc-ref record "PHONE")))
    (let loop ((phones (or phones '()))
	       (index 1)
	       (google-fields '()))
      (if (or (null? phones)
	      (> index 4))
	  google-fields
	  (loop (cdr phones)
		(1+ index)
		(acons (google-key->native (format #f "Phone ~a - Type" index))
		       (caar phones)
		       (acons (google-key->native (format #f
							  "Phone ~a - Value"
							  index))
			      (cdar phones)
			      google-fields)))))))

;; Write out a database in Google CSV format.
(define-method (native-write (db <google>) destination)

  (define (write-field field)
    (if (string-contains field "\n")
	(write field)
	(display field)))
  
  (define (write-record record)
    ;; Loop through fields.
    (let ((first #t))
      (for-each (lambda (key)
		  (or first
		      (display ","))
		  (write-field (or (assoc-ref record
					      (google-key->native key))
				   ""))
		  (set! first #f))
		keys)))

  ;; Write the keys.
  (write-record (map cons keys keys))

  ;; Loop through records.
  (for-each write-record (records db)))
