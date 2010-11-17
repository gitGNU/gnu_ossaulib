
(define-module (ossau backup)
  #:use-module (ossau filesys)
  #:use-module (ossau db hierarchical)
  #:use-module (ossau interactive)
  #:use-module (oop goops))

;; TO DO

;; Back up symbolic links by translating the link "parellel-ly" into
;; the backup file system.

;; Handle errors better - maybe automatically remove the failed file -
;; maybe query the user interactively for what to do.

;; Write reporting routines which read the backup database.

;; CODE

;; Backup record interface.
(define (br date device full? volumes)
  (vector date device full? volumes))
(define (br:date    r) (vector-ref r 0))
(define (br:device  r) (vector-ref r 1))
(define (br:full?   r) (vector-ref r 2))
(define (br:volumes r) (vector-ref r 3))

;; Backup file record interface.
(define (bfr name volume mtime ctime)
  (vector name volume mtime ctime))
(define (bfr:name   b) (vector-ref b 0))
(define (bfr:volume b) (vector-ref b 1))
(define (bfr:mtime  b) (vector-ref b 2))
(define (bfr:ctime  b) (vector-ref b 3))

;; When we fail to back up a file of this size or smaller, move on to
;; the next volume.
(define next-volume-size 50000)

;; Backup a single file.
(define (backup-file db name target volume full? file)
  (let* ((stat-info (lstat file))
         (mtime (stat:mtime stat-info))
         (ctime (stat:mtime stat-info))
         (db-recs (db-ref db file)))
    (display (string-append file ": "))
    (force-output)
    (cond
     ((eq? (stat:type stat-info) 'symlink)
      (display "symbolic link - ignored")
      (newline))
     ((or full?
          (not db-recs)
          (let* ((db-rec (car db-recs))
                 (db-mtime (bfr:mtime db-rec))
                 (db-ctime (bfr:ctime db-rec)))
            (or (> mtime db-mtime)
                (> ctime db-ctime))))
      (let ((backup-file-name (string-append target
                                             "/"
                                             name
                                             "-"
                                             (number->string volume)
                                             file)))
        (mkpath backup-file-name)
        (copy-file file backup-file-name)
        (db-set! db
                 file
                 (cons (bfr name volume mtime ctime)
                       (or db-recs '())))
        (display "backed up")
        (newline)))
     (else
      (db-set! db
               file
               (cons (bfr name #f mtime ctime)
                     (or db-recs '())))
      (display "unchanged")
      (newline)))))

;; The db key where backup records are stored.
(define backups-key-root "/BACKUPS/")

;; Backup a list of files.
(define (backup db name target full? files)
  (let* ((backups-key (string-append backups-key-root name))
         (backups-entry (db-ref db backups-key)))
    (if backups-entry
        (error "This backup name has been used previously!"))
    (set! backups-entry (br (current-time)
                            target
                            full?
                            '()))
    (let volume-loop ((volume 1) (files files))
      (let* ((label (begin
                      (display (string-append "Please insert a tape or disk for volume "
                                              (number->string volume)
                                              " of this backup,"))
                      (newline)
                      (display "and enter a label to identify that particular tape or disk: ")
                      (interactive-read-string ""))))
        (set! backups-entry (br (br:date backups-entry)
                                (br:device backups-entry)
                                (br:full? backups-entry)
                                (append (br:volumes backups-entry) (list label))))
        (db-set! db backups-key backups-entry)
        (let file-loop ((files files) (oversize-files '()))
          (or (eq? files '())
              (if (catch 'system-error
                         (lambda ()
                           (backup-file db name target volume full? (car files))
                           #t)
                         (lambda args
                           (display "error")
                           (newline)
                           #f))
                  ;; File backed up successfully - try the next one.
                  (file-loop (cdr files) oversize-files)
                  ;; File couldn't be backed up.
                  (if (or (<= (stat:size (lstat (car files))) next-volume-size)
                          (eq? (cdr files) '()))
                      ;; Move on to next volume.
                      (volume-loop (+ 1 volume) (append oversize-files files))
                      ;; Add this file to the oversize list and try backing up the next file.
                      (file-loop (cdr files) (append oversize-files (list (car files))))))))))))

;; Backup using standard db.
(define (backup-std name target full? files)
  (let ((db (make <db-hierarchical>)))
    (dynamic-wind
        (lambda () (db-open db "/home/backups"))
        (lambda () (backup db name target full? files))
        (lambda () (db-close db)))))

(export backup-file
        backup
        backup-std)

