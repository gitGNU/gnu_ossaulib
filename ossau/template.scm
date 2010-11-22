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

(define-module (ossau template)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:export (template->code)
  #:export-syntax (process-template))

;*****************************************************************************;
;* A template file is a file of content, such as HTML, that is complete      *;
;* except for places where the content needs to be filled in                 *;
;* programmatically.  In the case of the template processor code here, the   *;
;* code to fill in the dynamic content is written in Scheme and appears      *;
;* inline in the template file.                                              *;
;*                                                                           *;
;* Areas of Scheme code in the template file are delimited by $.  For        *;
;* example:                                                                  *;
;*                                                                           *;
;* <I>This page was processed by Guile $(display (version))$</I>             *;
;*                                                                           *;
;* here (display (version)) is interpreted and processed as Scheme code;     *;
;* the rest is normal (HTML) content.                                        *;
;*                                                                           *;
;* If either normal content or Scheme code needs to include a $ character,   *;
;* it can do so by doubling the $, as in: Price $$10.20.                     *;
;*                                                                           *;
;* Fragments of Scheme code do not have to be individually balanced.  For    *;
;* example:                                                                  *;
;*                                                                           *;
;* $(for-each (lambda (x)$                                                   *;
;* <LI>The square of $(display x)$ is $(display (* x x))$</LI>               *;
;* $          ) (iota 11))$                                                  *;
;*                                                                           *;
;* A shorthand is provided for cases where a fragment only wants to display  *;
;* a variable.  This is $~FORMAT VARNAME$, for example $~A x$.  ~FORMAT is a *;
;* format specifier understood by (ice-9 format), and VARNAME is the name of *;
;* the variable to display.                                                  *;
;*                                                                           *;
;* It may sometimes help to know the exact algorithm in order to write a     *;
;* piece of template file code correctly.  It is as follows.                 *;
;*                                                                           *;
;* 1. Convert the template file - even the normal content - into a big       *;
;*    Scheme code string by:                                                 *;
;*                                                                           *;
;*    - converting each fragment of normal content to `(display FRAGMENT)'   *;
;*                                                                           *;
;*    - converting each `~FORMAT VARNAME' fragment to                        *;
;*      `(format #t ~FORMAT VARNAME)'                                        *;
;*                                                                           *;
;*    - copying other Scheme code fragments as written.                      *;
;*                                                                           *;
;* 2. Read and evaluate this string in an environment as specified by the    *;
;*    arguments to process-template.                                         *;
;*                                                                           *;
;*****************************************************************************;

;*****************************************************************************;
;* template->code                                                            *;
;*                                                                           *;
;* Reads a template file and returns the Scheme code that should be read and *;
;* evaluated to generate the implied output.                                 *;
;*****************************************************************************;
(define (template->code template)
  ;***************************************************************************;
  ;* Utility procedure: convert any occurrences of "$$" in STRING to just    *;
  ;* "$".                                                                    *;
  ;***************************************************************************;
  (define (unescape-$$ string)
    (cond ((string-match "\\$\\$" string)
           =>
           (lambda (match-data)
             (string-append (substring string 0 (match:start match-data 0))
                            "$"
                            (unescape-$$ (substring string
                                                    (+ (match:start match-data
                                                                    0)
                                                       1))))))
          (else string)))
  ;***************************************************************************;
  ;* Utility procedure: given a string read from the template file, after    *;
  ;* splitting between scheme and non-scheme parts, return the Scheme code   *;
  ;* corresponding to the template string.                                   *;
  ;***************************************************************************;
  (define (make-code-string template-string in-scheme)
    (if in-scheme
        ;*********************************************************************;
        ;* Template string should be interpreted as Scheme code.  If it      *;
        ;* begins with "~", it is a shorthand for a format expression;       *;
        ;* otherwise, it is straight Scheme code and doesn't need any        *;
        ;* further tweaking.                                                 *;
        ;*********************************************************************;
        (cond ((string-match "^~[^ ]+ " template-string)
               =>
               (lambda (match-data)
                 (let ((beg (match:start match-data 0))
                       (end (match:end match-data 0)))
                   (format #f
                           "(format #t ~S ~A)"
                           (substring template-string beg (- end 1))
                           (substring template-string end)))))
              (else template-string))
        ;*********************************************************************;
        ;* Template string is normal file content (i.e. outside Scheme       *;
        ;* code).  The corresponding Scheme code should display it.          *;
        ;*********************************************************************;
        (format #f "(display ~S)" template-string)))
  ;***************************************************************************;
  ;* Main procedure code.                                                    *;
  ;***************************************************************************;
  (with-input-from-file template
    (lambda ()
      ;***********************************************************************;
      ;* Loop reading lines from the template file.                          *;
      ;***********************************************************************;
      (let loop ((template-line (read-line (current-input-port) 'concat))
                 (in-scheme #f)
                 (strings '()))
        (if (eof-object? template-line)
            ;*****************************************************************;
            ;* EOF: return the concatenated Scheme code string.              *;
            ;*****************************************************************;
;            (let ((code
            (string-append "(begin "
                           (apply string-append
                                  (reverse strings))
                           ")")
;            ))
;              (with-output-to-file "template-debug.scm"
;                (lambda ()
;                  (display code)))
;              code)
            ;*****************************************************************;
            ;* Not yet EOF: normal processing.  First check for single "$";  *;
            ;* these mark the boundaries between Scheme code and normal      *;
            ;* (non-Scheme) file content.                                    *;
            ;*****************************************************************;
            (cond ((string-match "(^|[^$])(\\$)($|[^$])" template-line)
                   =>
                   ;**********************************************************;
                   ;* Found a single "$", so process the part of the line    *;
                   ;* before the "$", then toggle the in-scheme flag and     *;
                   ;* loop to process the rest of the line.                  *;
                   ;**********************************************************;
                   (lambda (match-data)
                     (let (($pos (match:start match-data 2)))
                       (loop (let ((rest (substring template-line (+ $pos 1))))
                               (if (<= (string-length rest) 1)
                                   (read-line (current-input-port) 'concat)
                                   rest))
                             (not in-scheme)
                             (cons (make-code-string (unescape-$$
                                                      (substring template-line
                                                                 0
                                                                 $pos))
                                                     in-scheme)
                                   strings)))))
                  ;***********************************************************;
                  ;* No "$" in this line, so process whole line and loop to  *;
                  ;* read the next line.                                     *;
                  ;***********************************************************;
                  (else
                   (loop (read-line (current-input-port) 'concat)
                         in-scheme
                         (cons (make-code-string (unescape-$$ template-line)
                                                 in-scheme)
                               strings)))))))))

;*****************************************************************************;
;* process-template                                                          *;
;*                                                                           *;
;* Processes a template file, with the generated output going to the current *;
;* output port.  Returns unspecified.                                        *;
;*                                                                           *;
;* Args are: template     - Name of template file.                           *;
;*           vars         - Variables to define for the Scheme code in the   *;
;*                          template file, in the same form as a set of let  *;
;*                          bindings, i.e.                                   *;
;*                            ((variable1 value1)                            *;
;*                             (variable2 value2)                            *;
;*                             ...)                                          *;
;*           modules      - List of modules that the Scheme code in the      *;
;*                          template file uses.                              *;
;*                                                                           *;
;*****************************************************************************;
(define-macro (process-template template vars . modules)
  `(let ((module (make-module 31
                              (map resolve-interface
                                   ',modules))))
     ,@(map (lambda (vardef)
              `(module-define! module
                               ',(if (pair? vardef) (car vardef) vardef)
                               ,(if (pair? vardef) (cadr vardef) vardef)))
            vars)
     (eval (with-input-from-string (template->code ,template) read)
           module)))
